(ns n01se.wallframe
  (:require [clojure.repl :refer :all]
            [clojure.core.rrb-vector :as fv]
            [clojure.pprint :refer [pprint]]))

(def counted-frames
  [[1 34 44]
   [1 24.5 37]
   [1 10 10]
   [1 28.5 33.5]
   [1 25.5 31.5]
   [2 24 30]
   [3 23 19]
   [1 11.5 6.75]
   [1 30.5 36]
   [1 21 38.5]
   [4 8 10]
   [1 8.75 11.25]
   [1 8.75 10.75]
   [1 8 6.5]
   [1 7 8.75]
   [1 6 7.5]
   [1 13.5 16.5]
   [2 10 12]
   [1 19.5 23.25]
   [1 6 8.25]
   [1 5.5 7.5]
   [1 12.75 16.75]
   [1 14.5 26.5]
   [1 22.25 27.5]
   [1 15.25 19]

   ;; target:
   ;;[1 143 53]

   ])

;; target wall dimensions: width, height
(def target [148 53])
;;(def target (mapv #(* % 2.1) [75 90]))

;; The gap on every side of every frame should be 2-3 to be ideal.

(def frames (sort-by (fn [[w h]] (- (* w h)))
                     (mapcat (fn [[c x y]] (repeat c [x y])) counted-frames)))

(defn frame-margin [margin [w h]]
  [(+ w margin margin) (+ h margin margin)])

(defn placed-margin [margin [x1 y1 x2 y2]]
  [(- x1 margin) (- y1 margin) (+ x2 margin) (+ y2 margin)])

(defn overlap? [[ax1 ay1 ax2 ay2] [bx1 by1 bx2 by2]]
  (and (< ax1 bx2) (> ax2 bx1) (< ay1 by2) (> ay2 by1)))

(defn place [[width height] [x y]]
  [x y (+ x width) (+ y height)])

(defn rand-place [frame [target-width target-height]]
  (let [frame (if (< 0.5 (rand))
                frame
                [(frame 1) (frame 0)])]
    (place frame [(* (rand) (- target-width  (frame 0)))
                  (* (rand) (- target-height (frame 1)))])))

(defn rand-place-each [target frames]
  (reduce (fn [wall frame]
            (loop [i 500]
              (let [placed (rand-place frame target)]
                (if (some #(overlap? placed %) wall)
                  (if (zero? i)
                    wall
                    (recur (dec i)))
                  (conj wall placed)))))
          []
          frames))

(defn rand-attach [target wall [fw fh]]
  (let [[fw fh] (if (< 0.5 (rand)) [fw fh] [fh fw])
        oframe (rand-nth wall)
        x (nth oframe (rand-nth [0 2]))
        y (nth oframe (rand-nth [1 3]))
        x (if (< 0.5 (rand)) x (- x fw))
        y (if (< 0.5 (rand)) y (- y fh))]
    (place [fw fh] [x y])))

(defn rand-attach-each [target frames]
  (let [frames (shuffle frames)]
    (reduce (fn [wall frame]
              (loop [i 1000]
                (let [placed (rand-attach target wall frame)]
                  (if (or
                       (< (nth placed 0) 0)
                       (< (nth placed 1) 0)
                       (> (nth placed 2) (nth target 0))
                       (> (nth placed 3) (nth target 1))
                       (some #(overlap? placed %) wall))
                    (if (zero? i)
                      wall
                      (recur (dec i)))
                    (conj wall placed)))))
            [(rand-place (first frames) target)]
            (rest frames))))

(defn scatter [[tw th] flex frames]
  (loop [wall [], frames frames, remaining (* (- 1 flex) tw th)]
    (let [frames (filter #(< (* (% 0) (% 1)) remaining) frames)]
      (if (empty? frames)
        wall
        (let [r (rand-int (count frames))
              frame (nth frames r)]
          (recur (conj wall (rand-place frame [tw th]))
                 (concat (take r frames) (drop (inc r) frames))
                 (- remaining (* (frame 0) (frame 1)))))))))

(defn get-center [[x1 y1 x2 y2]]
  [(/ (+ x1 x2) 2) (/ (+ y1 y2) 2)])

(defn z=1 [x]
  (if (zero? x) 1 x))

(defn force-wall-1-step [target wall]
  (let [i (rand-int (count wall))
        frame (nth wall i)
        [cx cy] (get-center frame)
        overlaps (filter #(and (not= % frame) (overlap? frame %)) wall)]
    (if (empty? overlaps)
      wall
      (let [[dx dy] (reduce
                     (fn [[dx dy] oframe]
                       (let [[ox oy] (get-center oframe)]
                         [(+ dx (/ 5 (z=1 (- cx ox))))
                          (+ dy (/ 5 (z=1 (- cy oy))))]))
                     [0 0]
                     overlaps)
            ;;dx (+ (dec (* 2 (rand))) dx)
            ;;dy (+ (dec (* 2 (rand))) dy)
            dx (if (pos? dx)
                 (min dx  5 (- (target 0) (nth frame 2)))
                 (max dx -5 (- (nth frame 0))))
            dy (if (pos? dy)
                 (min dy  5 (- (target 1) (nth frame 3)))
                 (max dy -5 (- (nth frame 1))))
            new-frame [(+ dx (nth frame 0))
                       (+ dy (nth frame 1))
                       (+ dx (nth frame 2))
                       (+ dy (nth frame 3))]]
        (assoc wall i new-frame)))))

(def gfx-scale 5)

(defn new-window [target]
  (let [[tw th] (map #(* gfx-scale %) target)
        frame (doto (java.awt.Frame.)
                (.setVisible true)
                (.setSize (java.awt.Dimension. tw th)))
        _ (.addWindowListener frame (proxy [java.awt.event.WindowAdapter] []
                                      (windowClosing [we]
                                        (.dispose frame))))
        buffi (java.awt.image.BufferedImage. tw th (java.awt.image.BufferedImage/TYPE_INT_RGB))
        gfx (doto (.createGraphics buffi) (.setColor (java.awt.Color. 200 200 200)))]
    {:frame frame
     :buffi buffi
     :gfx gfx}))

(defn draw-wall [target {:keys [gfx frame buffi]} wall]
  (let [[tw th] (map #(* gfx-scale %) target)]
    (.clearRect gfx 0 0 tw th)
    (doseq [placed wall]
      (let [[x1 y1 x2 y2] (map #(* gfx-scale %) placed)]
        (.fillRect gfx x1 y1 (- x2 x1) (- y2 y1))))
    (.drawImage (.getGraphics frame) buffi 0 0 tw th nil)))

(def history (atom []))

(defn force-wall [target wall]
  (let [win (new-window target)]
    (loop [wall (vec wall,) i 10000]
      (draw-wall target win wall)
      ;;(Thread/sleep 30)
      (let [new-wall (force-wall-1-step target wall)]
        (swap! history conj new-wall)
        (if (pos? i)
          (recur new-wall (dec i))
          new-wall)))))

(defn area [[x1 y1 x2 y2]]
  (* (- x2 x1) (- y2 y1)))

(defn factor [cnt nf]
  (long (Math/floor (* nf cnt))))

(defn nthf [coll nf]
  (when (seq coll)
    (nth coll (factor (count coll) nf))))

(defn code-attach [target sorted-wall sorted-frames code]
  (let [{:keys [thisf otherf landscape? oleft? otop? tleft? ttop?]} code]
    (let [framei (factor (count sorted-frames) thisf)
          [f1 f2] (nth sorted-frames framei)
          new-frames (fv/catvec (fv/subvec sorted-frames 0 framei)
                                (fv/subvec sorted-frames (inc framei)))
          [fw fh] (if (or (and landscape? (>= f1 f2))
                          (and (not landscape?) (>= f2 f1)))
                    [f1 f2]
                    [f2 f1])
          oframe (or (nthf sorted-wall otherf)
                     (mapv double [(/ (target 0) 2) (/ (target 1) 2)
                                   (/ (target 0) 2) (/ (target 1) 2)]))
          x (nth oframe (if oleft? 0 2))
          y (nth oframe (if otop?  1 3))
          x (if tleft? x (- x fw))
          y (if ttop? y (- y fh))
          placed (place [fw fh] [x y])]
      (if (or
           (< (nth placed 0) 0)
           (< (nth placed 1) 0)
           (> (nth placed 2) (nth target 0))
           (> (nth placed 3) (nth target 1))
           (some #(overlap? placed %) sorted-wall))
        [sorted-wall sorted-frames]
        [(sort-by area (conj sorted-wall placed)) new-frames]))))

(defn rand-bool [] (< (rand) 0.5))

(defn rand-code []
  (let [oleft? (rand-bool)
        otop? (rand-bool)
        tleft? (rand-bool)
        ttop? (if (= oleft? tleft?) otop? (rand-bool))]
    {:thisf (rand)
     :otherf (rand)
     :landscape? (rand-bool)
     :oleft? oleft?
     :otop? otop?
     :tleft? tleft?
     :ttop? ttop?}))

(defn area-score [[tw th] wall]
  (- (* tw th) (reduce (fn [a placed] (+ a (area placed))) 0 wall)))

(defn score [_ wall]
  (- (reduce (fn [acc placed]
               (let [a (area placed)]
                 (+ acc a)))
             0 wall)))

(defn round [x]
  (let [r (/ (Math/round ^double (* x 100)) 100)]
    (if (or (instance? Long r) (zero? (denominator r)))
      r
      (double r))))

(defn write-wall [scale wall]
  (spit "wall.html"
        (apply str
               (for [placed wall
                     :let [[x1 y1 x2 y2] (map #(* % scale) placed)]]
                 (str
                  "<div style='width: " (- x2 x1)
                  "; height: " (- y2 y1)
                  "; top: " y1
                  "; left: " x1
                  "; border: 1px #000 solid; background: rgba(0,0,0,0.1); position: absolute"
                  "'>" (round (- (nth placed 2) (nth placed 0)))
                  "x"  (round (- (nth placed 3) (nth placed 1))) "</div>")))))

(defn write-wall-svg [wall]
  (spit "wall.svg"
        (str "<?xml version='1.0' encoding='UTF-8' standalone='no'?>"
             "<svg xmlns='http://www.w3.org/2000/svg' width='765' height='990' version='1.1'>"
             (apply str
                    (for [[x1 y1 x2 y2] wall]
                      (str"
    <g transform='translate(" x1 "," y1 ")'>
      <flowRoot
         transform='translate(1," (- y2 y1 1) ")'><flowPara
           style='font-size:2px;font-family:Arial'
           >" (round (- x2 x1)) "x" (round (- y2 y1)) "</flowPara></flowRoot>
      <rect x='0' y='0' height='" (- y2 y1) "' width='" (- x2 x1) "'
         style='fill:#000000;fill-opacity:0.1;stroke:#000000;stroke-opacity:1;stroke-width:0.1' />
    </g>")))
             "</svg>")))

(defn go1 []
  (let [[score wall]
        (reduce (fn [[old-score old-wall] _]
                  (let [wall (rand-place-each target (map #(frame-margin 1 %) frames))
                        score (score target wall)]
                    (if (< score old-score)
                      [score wall]
                      [old-score old-wall]))) [10000] (range 1000))]
    (prn score)
    (write-wall 5 (cons (list* 0 0 target) (map #(placed-margin -1 %) wall)))))

(defn go2 []
  (write-wall 5 (cons (list* 0 0 target)
                     (map #(placed-margin -1 %)
                          (force-wall
                           target
                           (scatter target 0.3 (map #(frame-margin 1 %)
                                                    (shuffle frames))))))))

(defn go3 []
  (time
   (let [final
         (apply max-key #(- (:score %))
                (pmap (fn [_]
                        (let [wall (rand-attach-each target (map #(frame-margin 1 %) frames))]
                          {:wall wall
                           :score (score target wall)})) (range 1000)))]
     (prn (:score final) (count (:wall final)))
     (write-wall-svg (cons (list* 0 0 target) (map #(placed-margin -1 %) (:wall final)))))))


(defn apply-codes [target frames codes]
  (first
   (reduce
    (fn [[wall frames] code]
      (try
        (code-attach target wall frames code)
        (catch Exception e
          (prn :bad-code code)
          (throw e))))
    [[] frames]
    codes)))

(defn score-pop [target frames pop]
  (sort-by second
           (pmap (fn [codes]
                   (let [wall (apply-codes target frames codes)]
                     [codes (score target wall)])) pop)))

(defn mutate-pop [pop]
  (let [best (take 100 pop)]
    (concat
     (for [disrupt (range 4)
           codes best]
       (reduce
        (fn [codes i]
          (assoc codes i (rand-code)))
        codes
        (take (* 20 disrupt) (shuffle (range (count codes))))))
     (for [_ (range 300)]
       (let [split (rand-int (count (first best)))]
         (into (subvec (rand-nth best) 0 split)
               (subvec (rand-nth best) split)))))))

(defn gengen [target frames pop n]
  (reduce
   (fn [pop i]
     (let [scored (score-pop target frames pop)]
       (prn :iter i :best (second (first scored)) :worst (second (last scored)))
       (write-wall 7 (cons (list* 0 0 target)
                           (map #(placed-margin -1 %)
                                (apply-codes target frames (ffirst scored)))))
       (mutate-pop (map first scored))))
   pop
   (range n)))

(defn go4 []
  (time
   (let [code-size 100
         frames (mapv #(frame-margin 1 %) frames)
         pop (repeatedly 1000 #(vec (repeatedly code-size rand-code)))
         final-pop (gengen target frames pop 200)
         wall (apply-codes target frames (first final-pop))]
     (prn :go4-done (score target wall) (count wall))
     (write-wall 7 (cons (list* 0 0 target) (map #(placed-margin -1 %) wall))))))

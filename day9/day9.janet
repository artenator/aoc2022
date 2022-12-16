(import ../utils/timer)

(defn determine-delta [dir steps]
  (map (partial * steps)
       (case dir
         "U" [0 1]
         "D" [0 -1]
         "L" [-1 0]
         "R" [1 0])))

(defn delta->dir [delta]
  (case delta
    [0 1] "U"
    [0 -1] "D"
    [-1 0] "L"
    [1 0] "R"))

(defn move-pos [[cur-x cur-y] new-dir steps]
  # (print cur-x " " cur-y " " new-dir " " steps)
  (let [[dx dy] (determine-delta new-dir steps)]
    [(+ cur-x dx) (+ cur-y dy)]))

(defn clamp [delta-unit]
  (let [clamp-fn (cond
                   (pos? delta-unit) (partial min 1)
                   (neg? delta-unit) (partial max -1)
                   (zero? delta-unit) identity)]
    (clamp-fn delta-unit)))

(defn positions->delta [[ax ay] [bx by]]
  (let [delta [(- bx ax) (- by ay)]]
    delta))

(defn move-pos-2 [a b]
  (let [delta (positions->delta a b)
        [dx dy] (map clamp delta)
        [ax ay] a]
    [(+ ax dx) (+ ay dy)]))

(defn coords [[xs xe] [ys ye]]
  (seq [y :range [ys ye]
        x :range [xs xe]]
    [x y]))

(def pattern
  ~{:p (drop (cmt ($) ,(fn [n] (print "AT: " n) n)))
    :move (group
            (* (+ (-> :last-dir) (constant nil))
               (<- (+ "U" "D" "L" "R") :last-dir)
               " "
               (/ (<- :d+)
                  ,scan-number
                  :steps)
               (/ (* (+ (-> :last-pos) (constant [0, 0])) (-> :last-dir) (-> :steps))
                  ,move-pos
                  :last-pos)
               "\n"))
    ,move-pos
    :cur-pos
    :main (* :p (some :move) -1)})

(defn do-moves [cur-pos dir steps]
  (reduce (fn [positions delta]
            [;positions (move-pos (or (last positions) cur-pos) dir 1)])
          []
          (range steps)))

(defn create-step-units [dir steps last-step-units]
  #(printf "%Q %Q %Q" dir steps last-step-units)
  (let [[prev-last-dir prev-dir prev-pos] (last last-step-units)
        steps (do-moves prev-pos dir steps)]
    #(pp steps)
    (map (partial tuple prev-dir dir) steps)))

(def pattern-2
  ~{:p (drop (cmt ($) ,(fn [n] (print "AT: " n) n)))
    :move (group
            (/ (* (<- (+ "U" "D" "L" "R") :last-dir)
                  " "
                  (/ (<- :d+)
                     ,scan-number
                     :steps)
                  (+ (-> :last-step-units) (constant [[nil nil [0, 0]]]))
                  "\n")
               ,create-step-units
               :last-step-units))
    ,move-pos
    :cur-pos
    :main (* :p (some :move) -1)})

(def pattern-3
  ~{:p (drop (cmt ($) ,(fn [n] (print "AT: " n) n)))
    :move (group
            (/ (* (<- (+ "U" "D" "L" "R") :last-dir)
                  " "
                  (/ (<- :d+)
                     ,scan-number
                     :steps)
                  (+ (-> :last-step-units) (constant [[nil nil [0, 0]]]))
                  "\n")
               ,create-step-units
               :last-step-units))
    ,move-pos
    :cur-pos
    :main (* :p (some :move) -1)})

(defn near-me? [[cur-x cur-y] target-pos]
  # (printf "@near-me? %Q %Q" [cur-x cur-y] target-pos)
  (let [coords-around-me (coords [(dec cur-x) (+ cur-x 2)] [(dec cur-y) (+ cur-y 2)])
        around-me (zipcoll coords-around-me
                           (array/new-filled (length coords-around-me) true))]
    # (printf "around? %Q %Q" around-me [cur-x cur-y])
    # (printf "around-true? %Q %Q" (in around-me target-pos) target-pos)
    (in around-me target-pos)))

(defn main []
  (let [contents (slurp "./day9/input.txt")
        parsed (->> (peg/match pattern-2 contents)
                    (apply array/concat)
                    (apply array/concat))]
    (pp parsed)
    (let [tail-pos @[[0 0]]
          visited @{[0 0] true}]
      (each [prev-dir dir cur-pos] parsed
        (when (not (near-me? cur-pos (first tail-pos)))
          (let [opposite-dir (case dir
                               "U" "D"
                               "D" "U"
                               "L" "R"
                               "R" "L")
                ideal-placement (move-pos cur-pos opposite-dir 1)]
            (set (tail-pos 0) ideal-placement)
            (put visited ideal-placement true))))
      (print (length visited)))
    (let [tail-pos (array/new-filled 9 [0 0])
          visited @{[0 0] true}]
      (each [prev-dir dir cur-pos] parsed
        (let [prev-tail (array ;tail-pos)]
          (for i 0 (length tail-pos)
            (let [cur-knot (if (= i 0)
                             cur-pos
                             (get tail-pos (dec i)))
                  prev-knot (get tail-pos i)]
              (when (not (near-me? cur-knot prev-knot))
                (let [opposite-dir (case dir
                                     "U" "D"
                                     "D" "U"
                                     "L" "R"
                                     "R" "L")
                      ideal-placement (move-pos-2 prev-knot cur-knot)]
                  (set (tail-pos i) ideal-placement)
                  (when (= i 8)
                    (put visited ideal-placement true))))))))
      #(pp visited)
      (print (length visited)))))

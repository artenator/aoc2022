(import ../utils/timer)

(def pattern
  ~{:row (* (group (some (number :d)))
            "\n")
    :main (* (some :row) -1)})

(defn trees-in-dir [grid x y dir]
  (case dir
    :left (slice (get grid y) 0 x)
    :right (slice (get grid y) (inc x) (length (first grid)))
    :top (map |(get-in grid [$ x]) (range 0 y))
    :bottom (map |(get-in grid [$ x]) (range (inc y) (length grid)))))

(def dirs [:left :right :top :bottom])

(defn greater-or-empty [tree-height tree-dirs]
  (every? (map (partial > tree-height) tree-dirs)))

(defn tree-visible? [grid x y]
  (let [tree-height (get-in grid [y x])]
    (->> dirs
         (map (partial trees-in-dir grid x y))
         (map (partial greater-or-empty tree-height))
         (any?))))

(defn scenic-num [grid x y]
  (let [tree-height (get-in grid [y x])
        trees-in-dir (map (partial trees-in-dir grid x y) dirs)]
    (map (fn [idx trees]
           (if-let [bigger-tree-idx (find-index |(>= $ tree-height) (case idx
                                                                      0 (reverse trees)
                                                                      2 (reverse trees)
                                                                      trees))]
             (inc bigger-tree-idx)
             (length trees)))
         (range (length trees-in-dir))
         trees-in-dir)))

(defn coords [[xs xe] [ys ye]]
  (seq [y :range [ys ye]
        x :range [xs xe]]
    [x y]))

(defn main []
  (let [content (slurp "./day8/input.txt")
        parsed (peg/match pattern content)
        y-range [1 (dec (length parsed))]
        x-range [1 (dec (length (get parsed 0)))]]
    (timer/with-timer
      (let [visible (reduce (fn [acc [x y]]
                              (if (tree-visible? parsed x y)
                                (inc acc)
                                acc))
                            (+ (* 2 (length parsed))
                               (* 2 (- (length (get parsed 0)) 2)))
                            (coords x-range y-range))]
        (printf "total visible %Q" visible)))
    (timer/with-timer
      (let [scenic-score (map (fn [[x y]] (scenic-num parsed x y))
                              (coords x-range y-range))]
        (printf "total scenic score %Q" (->> scenic-score
                                             (map (partial apply *))
                                             (max-of)))))))

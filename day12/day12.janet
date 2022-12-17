(import ../utils/grid)

(def pattern
  ~{:row (* (group
              (some (+ (/ (<- (range "az"))
                          ,(comp |(- $ 97) first string/bytes))
                       (/ (* (column) (line) (<- (set "SE")) (-> :start-end-pos))
                          ,(fn [x y start-or-end cur-positions]
                             (case start-or-end
                               "S" (set (cur-positions :start-pos) [(dec x) (dec y)])
                               "E" (set (cur-positions :end-pos) [(dec x) (dec y)]))
                             -1))))) "\n")
    :main (* (constant @{} :start-end-pos) (group (some :row)) -1)})

(defn surrounding-point [coord]
  (map |(grid/move-pos coord $ 1) [:left :right :up :down]))

(defn climbing-search [start end grid]
  (let [visited @{start true}
        q @[[start @[]]]
        possible-paths @[]
        valid-step? (fn [cur dest cur-path]
                      (let [[dest-x dest-y] dest
                            [cur-x cur-y] cur]
                        (and (< -1 dest-x (length (get grid 0)))
                             (< -1 dest-y (length grid))
                             (not (in visited dest))
                             (>= (- (get-in grid [cur-y cur-x]) (get-in grid [dest-y dest-x])) -1))))]
    (while (not (empty? q))
      (let [[[cur cur-path]] q
            new-coords (surrounding-point cur)]
        (array/remove q 0)
        (if (= cur end)
          (do
            (array/push possible-paths @[;cur-path cur])
            (break))
          (map (fn [new-coord]
                 (when (valid-step? cur new-coord cur-path)
                   (set (visited new-coord) true)
                   (array/push q [new-coord @[;cur-path cur]]))) new-coords))))
    (when (not (empty? possible-paths))
      (length (first possible-paths)))))

(defn main []
  (let [contents (slurp "./day12/input.txt")
        [{:start-pos start :end-pos end} grid] (peg/match pattern contents)
        [max-end] (->> end
                       (surrounding-point)
                       (map (fn [coord]
                              (let [[x y] coord]
                                [coord (get-in grid [y x])])))
                       (sort-by |(get $ 1))
                       (last))
        x-len (length grid)
        y-len (length (get grid 0))
        y-range [0 x-len]
        x-range [0 y-len]
        starts [start ;(->> (grid/coords x-range y-range)
                            (map (fn [[x y]] [[x y] (get-in grid [y x])]))
                            (filter (fn [[[x y] val]]
                                      (and (or (= x 0)
                                               (= y 0)
                                               (= x (dec x-len))
                                               (= y (dec y-len)))
                                           (= val 0))))
                            (map first))]]
    (printf "parsed start end %q %q" start end)
    (printf "end %Q" max-end)
    (print "part 1:")
    (pp (climbing-search start max-end grid))
    (print "part 2:")
    (pp starts)
    (pp (min ;(->> starts
                   (map |(climbing-search $ max-end grid))
                   (filter identity))))))

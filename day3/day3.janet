(defn create-set [s]
  (->> s
       (string/bytes)
       (frequencies)
       (table/to-struct)))

(defn gen-ascii-score [ch]
  (if (> ch 96)
    (- ch 96)
    (- ch 38)))

(defn find-common-ch [char-set & char-sets]
  (var common-ch nil)
  (eachk ch char-set
    (when (every? (map |(get $ ch) char-sets))
      (set common-ch ch)
      (break)))
  common-ch)

(defn half-it [s]
  (let [half-point (/ (length s) 2)
        first-half (string/slice s 0 half-point)
        second-half (string/slice s half-point -1)]
    [first-half second-half]))

(defn score-it [rucksack-strs]
  (let [unique-chs (map (partial create-set) rucksack-strs)]
    (-> (find-common-ch ;unique-chs)
        (gen-ascii-score))))

(def peg
  ~{:rucksack (* (/ (<- (some (range "az" "AZ"))) ,|(-> $
                                                        (half-it)
                                                        (score-it))) "\n")
    :main (* (some :rucksack) -1)})

(def peg-2
  ~{:rucksack (repeat 3 (* (<- (some (range "az" "AZ"))) "\n"))
    :main (* (some (/ (group :rucksack) ,score-it)) -1)})

(defn main []
  (let [contents (slurp "./day3/input.txt")
        compartments (peg/match peg contents)
        compartments-2 (peg/match peg-2 contents)]
    (print "priority sum 1: " (sum compartments))
    (print "priority sum 2: " (sum compartments-2))))

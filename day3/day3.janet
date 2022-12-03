(defn create-set [s]
  (var acc @{})
  (loop [ch :in s]
    (let [char (string/from-bytes ch)
          existing (get acc char)]
      (if (not existing)
        (set (acc char) 1)
        (set (acc char) (inc existing)))))
  acc)

(defn score-it [ch]
  (let [[ascii-num] (string/bytes ch)]
    (if (> ascii-num 96)
      (- ascii-num 96)
      (- ascii-num 38))))

(defn half-it [s]
  (let [half-point (/ (length s) 2)
        first-half (-> (string/slice s 0 half-point)
                       (create-set))
        second-half (-> (string/slice s half-point -1)
                        (create-set))]
    (var common-ch nil)
    (loop [ch :keys first-half]
      (when (get second-half ch)
        (set common-ch ch)
        (break)))
    (score-it common-ch)))

(def peg
  ~{:rucksack (* (/ (<- (some (range "az" "AZ"))) ,half-it) "\n")
    :main (* (some :rucksack) -1)})

(def peg-2
  ~{:rucksack (repeat 3 (* (<- (some (range "az" "AZ"))) "\n"))
    :main (* (some (/ (group :rucksack) ,(fn [[s1 s2 s3]]
                                           (let [s1-unique (create-set s1)
                                                 s2-unique (create-set s2)
                                                 s3-unique (create-set s3)]
                                             (var common-ch nil)
                                             (loop [k :keys s1-unique]
                                               (when (and (get s1-unique k)
                                                          (get s2-unique k)
                                                          (get s3-unique k))
                                                 (set common-ch k)
                                                 (break)))
                                             (score-it common-ch)))))
             -1)})

(defn main []
  (let [contents (slurp "./day3/input.txt")
        compartments (peg/match peg contents)
        compartments-2 (peg/match peg-2 contents)]
    (print "priority sum 1: " (sum compartments))
    (print "priority sum 2: " (sum compartments-2))))

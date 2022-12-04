(defn has-coverage [s1 e1 s2 e2]
  (or (<= s1 s2 e2 e1)
      (<= s2 s1 e1 e2)))

(defn has-coverage-2 [s1 e1 s2 e2]
  (or (<= s1 s2 e1)
      (<= s2 s1 e2)))

(defn pattern [handler]
  ~{:range (* (number :d+) "-" (number :d+))
    :main (* (some (group (/ (*  :range "," :range "\n") ,handler))) -1)})

(defn main []
  (let [contents (slurp "./day4/input.txt")
        ranges (peg/match (pattern has-coverage) contents)
        ranges-2 (peg/match (pattern has-coverage-2) contents)]
    (print "1: " (length (filter (comp (partial = true) first) ranges)))
    (print "2: " (length (filter (comp (partial = true) first) ranges-2)))))

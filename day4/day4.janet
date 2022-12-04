(defn has-coverage [s1 e1 s2 e2]
  (or (<= s1 s2 e2 e1)
      (<= s2 s1 e1 e2)))

(defn has-coverage-2 [s1 e1 s2 e2]
  (or (<= s1 s2 e1)
      (<= s2 s1 e2)))

(def peg
  ~{:range (* (number :d+) "-" (number :d+))
    :main (* (some (group (/ (*  :range "," :range "\n") ,has-coverage))) -1)})

(def peg-2
  ~{:range (* (number :d+) "-" (number :d+))
    :main (* (some (group (/ (*  :range "," :range "\n") ,has-coverage-2))) -1)})

(defn main []
  (let [contents (slurp "./day4/input.txt")
        ranges (peg/match peg contents)
        ranges-2 (peg/match peg-2 contents)]
    (print "1: " (length (filter (comp (partial = true) first) ranges)))
    (print "2: " (length (filter (comp (partial = true) first) ranges-2)))))

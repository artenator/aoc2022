(defn capture-if-unique [num-of-chars s cur-pos]
  (when (-> s
            (frequencies)
            (keys)
            (length)
            (= num-of-chars))
    cur-pos))

(defn pattern [num-of-chars]
  ~{:try-unique-chars (if (> 0 (cmt (* (<- (at-most ,num-of-chars :a)) ($))
                                    ,(partial capture-if-unique num-of-chars)))
                        (<- (at-most ,num-of-chars :a) :match-found))
    :main (* (some (if-not (-> :match-found)
                     (+ :try-unique-chars
                        (? 1))))
             (any :a)
             "\n"
             -1)})

(defn main []
  (let [contents (slurp "./day6/input.txt")]
    (printf "part 1: %Q" (peg/match (pattern 4) contents))
    (printf "part 2: %Q" (peg/match (pattern 14) contents))))

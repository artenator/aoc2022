
(def elf-peg
  ~{:calories (* (number :d+) "\n")
    :elf (+ (group (some :calories)) "\n")
    :main (* (some :elf) -1)})

(defn main []
  (let [contents (slurp "./day1/input.txt")
        elf-matches (peg/match elf-peg contents)
        elf-calories (map sum elf-matches)]
    (print "top elf: " (apply max elf-calories))
    (print "top three elves: " (->> (sort elf-calories >)
                                    (take 3)
                                    (sum)))))

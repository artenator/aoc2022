(defn move-to [stacks from to c &opt order]
  (default order :maintain)
  (let [src-stack (get stacks from)
        dest-stack (get stacks to)]
    (if (= order :maintain)
      (do
        (array/push dest-stack ;(array/slice src-stack (- (length src-stack) c)))
        (for i 0 c
          (array/pop src-stack)))
      (for i 0 c
        (array/push dest-stack (array/pop src-stack))))
    stacks))

(defn build-stacks [stack-pairs]
  (reduce (fn [acc [col val]]
            (if-let [stack (get acc col)]
              (update acc col |(array/push $ val))
              (do
                (set (acc col) @[val])
                acc)))
          @[]
          (reverse stack-pairs)))

(defn process-stacks [order instructions built-stacks]
  (reduce (fn [stacks @{:to to :from from :count c}]
            (move-to stacks from to c order))
          built-stacks
          instructions))

(defn get-tops-of-stacks [stacks]
  (map last (values stacks)))

(def pattern
  ~{:stack-unit (* (+ (group (* (/ (column) ,|(/ (dec $) 4)) "[" (<- :a) "]"))
                      (* " " :d (? " "))
                      (repeat 3 " "))
                   (? (set "\n ")))
    :stacks (* (group (some :stack-unit)))
    :instructions (group
                    (some (/  (* "move " (* (constant :count) (number :d+))
                                 " from " (* (constant :from) (/ (number :d+) ,dec))
                                 " to " (* (constant :to) (/ (number :d+) ,dec)) "\n")
                              ,struct)))
    :main (* (/ :stacks ,build-stacks)
             "\n"
             :instructions
             -1)})

(defn main []
  (let [contents (slurp "./day5/input.txt")
        [p1 inst1] (peg/match pattern contents)
        [p2 inst2] (peg/match pattern contents)]
    (print "part 1: " (string/join (->> p1
                                        (process-stacks :reg inst1)
                                        (get-tops-of-stacks))
                                   ""))
    (print "part 2: " (string/join (->> p2
                                        (process-stacks :maintain inst2)
                                        (get-tops-of-stacks))
                                   ""))))

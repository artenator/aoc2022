(defn build-stacks [stack-pairs]
  (reduce (fn [acc [col val]]
            (if-let [stack (get acc col)]
              (update acc col |(array/push $ val))
              (do
                (set (acc col) @[val])
                acc)))
          @[]
          (reverse stack-pairs)))

(defn process-stacks [instructions built-stacks]
  (reduce (fn [stacks @{:to to :from from :count c}]
            (let [cur-stack (get stacks from)
                  dest-stack (get stacks to)]
              (for i 0 c
                (array/push dest-stack (array/pop cur-stack)))
              stacks))
          built-stacks
          instructions))

(defn process-stacks-2 [instructions built-stacks]
  (reduce (fn [stacks @{:to to :from from :count c}]
            (let [cur-stack (get stacks from)
                  dest-stack (get stacks to)]
              (array/push dest-stack ;(array/slice cur-stack (- (length cur-stack) c)))
              (for i 0 c
                (array/pop cur-stack))
              stacks))
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
                                        (process-stacks inst1)
                                        (get-tops-of-stacks))
                                   ""))
    (print "part 2: " (string/join (->> p2
                                        (process-stacks-2 inst2)
                                        (get-tops-of-stacks))
                                   ""))))

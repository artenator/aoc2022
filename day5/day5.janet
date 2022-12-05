(defn build-stacks [stack-pairs]
  (reduce (fn [acc [col val]]
            (if-let [stack (get acc col)]
              (do
                (array/push stack val)
                acc)
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

(defn pattern [handler]
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
    :main (* (/ :stacks ,build-stacks :built-stacks)
             "\n"
             (/ (* :instructions (-> :built-stacks)) ,handler)
             -1)})

(defn main []
  (let [contents (slurp "./day5/input.txt")
        [_ sol] (peg/match (pattern process-stacks) contents)
        [_ sol-2] (peg/match (pattern process-stacks-2) contents)]
    (print "part 1: " (string/join (get-tops-of-stacks sol) ""))
    (print "part 2: " (string/join (get-tops-of-stacks sol-2) ""))))

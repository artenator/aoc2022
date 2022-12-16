(defn int-/ [x y]
  (-> x (/ y) (math/floor)))

(defn prime-factors [n]
  (def factors @[])
  (var val (if (string? n) (scan-number n) n))
  (while (zero? (% val 2))
    (array/push factors 2)
    (set val (int-/ val 2)))
  (each i (range 3 (math/floor (math/sqrt val)) 2)
    (while (zero? (% val i))
      (array/push factors i)
      (set val (int-/ val i))))
  (when (> val 2)
    (array/push factors val))
  factors)

(def pattern
  ~{:num-list (group (any (* (number :d+) (? ", "))))
    :monkey (/ (group
                 (* "Monkey " (number :d+) ":\n"
                    :s+ "Starting items: " :num-list "\n"
                    :s+ "Operation: new = " (/ (* (+ (number :d+)
                                                     (<- "old"))
                                                  :s
                                                  (<- (set "*+"))
                                                  :s
                                                  (+ (number :d+)
                                                     (<- "old")))
                                               ,(fn [x op y]
                                                  (let [o (case op
                                                            "*" *
                                                            "+" +)]
                                                    (fn [old]
                                                      (o (if (string? x)
                                                           old
                                                           x)
                                                         (if (string? y)
                                                           old
                                                           y)))))) "\n"
                    :s+ "Test: divisible by " (/ (number :d+)
                                                 ,(fn [n]
                                                    |(zero? (% $ n))))  "\n"
                    :s+ "If true: throw to monkey " (number :d+) "\n"
                    :s+ "If false: throw to monkey " (number :d+) "\n"))
               ,(partial zipcoll
                         [:id :current-items :operation :test :if-true :if-false]))
    :main (* (some (* :monkey (? "\n"))) -1)})

(defn super-mod [vals]
  |(% $ (product vals)))

(def pattern-mods
  ~{:divisions (* (thru "divisible by ") (number :d+) "\n")
    :main (* (some :divisions))})

(defn pattern-2 [sm]
  ~{:num-list (group (any (* (number :d+) (? ", "))))
    :monkey (/ (group
                 (* "Monkey " (number :d+) ":\n"
                    :s+ "Starting items: " :num-list "\n"
                    :s+ "Operation: new = " (/ (* (+ (number :d+)
                                                     (<- "old"))
                                                  :s
                                                  (<- (set "*+"))
                                                  :s
                                                  (+ (number :d+)
                                                     (<- "old")))
                                               ,(fn [x op y]
                                                  (let [o (case op
                                                            "*" *
                                                            "+" +)]
                                                    (fn [old]
                                                      (sm
                                                        (o (if (string? x)
                                                             old
                                                             x)
                                                           (if (string? y)
                                                             old
                                                             y))))))) "\n"
                    :s+ "Test: divisible by " (/ (number :d+)
                                                 ,(fn [n]
                                                    |(zero? (% $ n))))  "\n"
                    :s+ "If true: throw to monkey " (number :d+) "\n"
                    :s+ "If false: throw to monkey " (number :d+) "\n"))
               ,(partial zipcoll
                         [:id :current-items :operation :test :if-true :if-false]))
    :main (* (some (* :monkey (? "\n"))) -1)})

(defn main []
  (let [contents (slurp "./input.txt")
        parsed (peg/match pattern contents)
        my-super-mod (super-mod (peg/match pattern-mods contents))
        parsed-2 (peg/match (pattern-2 my-super-mod) contents)
        inspection-count (zipcoll (range (length parsed))
                                  (array/new-filled (length parsed) 0))
        inspection-count-2 (zipcoll (range (length parsed-2))
                                    (array/new-filled (length parsed-2) 0))]
    (for i 0 20
      (each {:current-items items
             :operation op
             :test test
             :if-true it
             :if-false if
             :id id} parsed
        (while (> (length items) 0)
          (let [cur-worry-level (items 0)
                new-worry-level (-> (op cur-worry-level)
                                    (/ 3)
                                    (math/floor))
                target-monkey-idx (if (test new-worry-level) it if)
                {:current-items target-items} (parsed target-monkey-idx)]
            (array/remove items 0)
            (array/push target-items new-worry-level)
            (update inspection-count id inc)))))
    (pp inspection-count)
    (printf "part 1: %Q" (-> inspection-count
                             (values)
                             (sorted >)
                             (|(take 2 $))
                             (product)))
    (pp parsed-2)
    (for i 0 10000
      (each {:current-items items
             :operation op
             :test test
             :if-true it
             :if-false if
             :id id} parsed-2
        (while (> (length items) 0)
          (let [cur-worry-level (items 0)
                new-worry-level (op cur-worry-level)
                target-monkey-idx (if (test new-worry-level) it if)
                {:current-items target-items} (parsed-2 target-monkey-idx)]
            (array/remove items 0)
            (array/push target-items new-worry-level)
            (update inspection-count-2 id inc)))))

    (pp parsed-2)
    (pp inspection-count-2)
    (printf "part 2: %Q" (-> inspection-count-2
                             (values)
                             (sorted >)
                             (|(take 2 $))
                             (product)))))

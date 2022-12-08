(import ../utils/timer)

(defn move-dir [cur-dir cur-move]
  (let [cur-dir (if (= cur-dir "") "/" cur-dir)]
    (cond
      (= (slice cur-move 0 1) "/")
      cur-move
      (= cur-move "..")
      (let [potential-new-dir (slice cur-dir 0 (last (string/find-all "/" cur-dir)))
            new-dir (if (> (length potential-new-dir) 0)
                      potential-new-dir
                      "/")]
        new-dir)
      (string cur-dir (when (not= cur-dir "/") "/") cur-move))))

(defn move-dirs [cur-dir cur-moves]
  (reduce move-dir cur-dir cur-moves))

(def pattern
  ~{:file (* (number :d+) " " (<- (to "\n")))
    :dir (* "dir " :a+)
    :ls-out (group (* (-> :cur-dir) (group (some (*  (+ :dir :file) "\n")))))
    :cd-out ""
    :out (+ :ls-out :cd-out)
    :cd (* "cd " (/ (*  (+ (-> :cur-dir) (<- "")) (group (some (<- (* (? "/") (? (to "\n")))))))
                    ,move-dirs
                    :cur-dir))
    :ls (* "ls")
    :cmd (* "$ " (+ :cd :ls) "\n")
    :main (/ (* (some (+ :cmd :out)) -1) ,(fn [& matches]
                                            (->> (array ;matches)
                                                 (filter array?))))})

(defn add-to-directory [size tree dir-path]
  (update tree dir-path (fn [cur-size]
                          (+ (or cur-size 0) size))))

(defn generate-dir-paths [dir-path]
  (if (= dir-path "/")
    [["/"]]
    (let [separate-dirs (string/split "/" (slice dir-path 1))]
      (map (fn [idx]
             ["/" ;(slice separate-dirs 0 idx)])
           (range 0 (inc (length separate-dirs)))))))

(defn main
  []
  (let [content (slurp "./day7/input.txt")
        [dir-contents] (peg/match pattern content)
        dir-structure (reduce (fn [acc [cur-dir files]]
                                (let [file-sizes (filter number? files)
                                      sum-sizes (sum file-sizes)
                                      dir-paths (generate-dir-paths cur-dir)]
                                  (reduce (partial add-to-directory sum-sizes) acc dir-paths)))
                              @{}
                              dir-contents)]
    (pp dir-contents)
    (pp dir-structure)
    (timer/with-timer
      (printf "part 1: %Q" (->> dir-structure
                                (values)
                                (filter |(< $ 100000))
                                (sum))))
    (timer/with-timer
      (printf "part 2: %Q" (let [used-space (get dir-structure '("/"))
                                 space-per-dir (->> dir-structure
                                                    (values)
                                                    (sorted))
                                 unused-space (- 70000000 used-space)]
                             (some |(and (> $ (- 30000000 unused-space)) $) space-per-dir))))))

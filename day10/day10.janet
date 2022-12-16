(def instr-cycles
  {"addx" 2
   "noop" 1})

(defn parse-instr [instr & args]
  (let [cycles (instr-cycles instr)]
    {:instr instr :cycles cycles :args args}))

(def pattern
  ~{:p (drop (cmt ($) ,(fn [n] (print "AT: " n) n)))
    :add (/ (* (<- "addx") " " (number (to "\n"))) ,parse-instr)
    :noop (/ (<- "noop") ,parse-instr)
    :inst (* (+ :add :noop) "\n")
    :main (* (some :inst) -1)})

(defn instr-handler [cpu instr args]
  (when (= "addx" instr)
    (update cpu :x (partial + (first args)))))

(defn sprite-location [cycle]
  (from-pairs (map (fn [pos] [pos true]) (range (dec cycle) (+ cycle 2)))))

(setdyn :debug true)

(defn main []
  (let [contents (slurp "./day10/input.txt")
        parsed (peg/match pattern contents)]
    (let [cpu @{:pc 0
                :cycles 0
                :x 1}
          cpu-state-by-cycle @[]
          drawing @[]]
      (printf "cur cpu state %Q" cpu)
      (each {:cycles c :instr instr :args a} parsed
        (for i 0 c
          (array/push drawing (if (in (sprite-location (cpu :x)) (% (cpu :cycles) 40))
                                "#"
                                "."))
          (update cpu :cycles inc)
          (array/push cpu-state-by-cycle (table/clone cpu))
          (printf "cur cpu state %Q" cpu))
        (instr-handler cpu instr a))
      # (array/push cpu-state-by-cycle (table/clone cpu))
      # (update cpu :cycles inc)
      (printf " %Q" (map |(get cpu-state-by-cycle $) [20 60 100 140 180 220]))
      (printf "240 %Q" (get cpu-state-by-cycle 239))
      (let [scores (map (fn [cycle]
                          (let [{:x x} (get cpu-state-by-cycle (dec cycle))]
                            (* cycle x)))
                        [20 60 100 140 180 220])]
        (printf "part 1: %Q" (sum scores))
        (each row (partition 40 drawing)
          (print (string/join row "")))))))

(debug/fbreak instr-handler)

(defn rps-converter [alpha]
  (case alpha
    "A" 0
    "B" 1
    "C" 2
    "X" 0
    "Y" 1
    "Z" 2))

(defn win-loss-draw [elf me]
  (cond
    (= elf (mod (inc me) 3)) 0
    (= elf (mod (dec me) 3)) 6
    (= elf me) 3))

(defn win-loss-draw-2 [elf me]
  (case me
    0 0
    1 3
    2 6))

(defn my-free-points-2 [me elf]
  (inc (case me
         0 (mod (dec elf) 3)
         1 elf
         2 (mod (inc elf) 3))))

(def peg
  ~{:me (/ (<- (range "XZ")) ,rps-converter :my-turn)
    :elf (/ (<- (range "AC")) ,rps-converter :elf-turn)
    :round-points (/ (* (-> :elf-turn) (-> :my-turn)) ,win-loss-draw)
    :my-free-points (/ (-> :my-turn) ,inc)
    :round (* :elf " " :me :my-free-points :round-points "\n")
    :main (* (some (* (group :round))) -1)})

(def peg-2
  ~{:me (/ (<- (range "XZ")) ,rps-converter :my-turn)
    :elf (/ (<- (range "AC")) ,rps-converter :elf-turn)
    :round-points (/ (* (-> :elf-turn) (-> :my-turn)) ,win-loss-draw-2)
    :my-free-points (/ (* (-> :my-turn) (-> :elf-turn)) ,my-free-points-2)
    :round (* :elf " " :me :my-free-points :round-points "\n")
    :main (* (some (* (group :round))) -1)})

(defn calculate-total-score [game-scores]
  (sum (mapcat (fn [[_ _ free-points win-loss-points]]
                 (+ free-points win-loss-points))
               game-scores)))

(defn main []
  (let [content (slurp "./day2/input.txt")
        game-scores (peg/match peg content)
        game-scores-2 (peg/match peg-2 content)
        my-score (calculate-total-score game-scores)
        my-score-2 (calculate-total-score game-scores-2)]
    (print "my score: " my-score)
    (print "my score 2: " my-score-2)))

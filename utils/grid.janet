(defn determine-delta [dir steps]
  (map (partial * steps)
       (case dir
         :up [0 1]
         :down [0 -1]
         :left [-1 0]
         :right [1 0])))

(defn move-pos [[cur-x cur-y] new-dir steps]
  (let [[dx dy] (determine-delta new-dir steps)]
    [(+ cur-x dx) (+ cur-y dy)]))

(defn coords [[xs xe] [ys ye]]
  (seq [y :range [ys ye]
        x :range [xs xe]]
    [x y]))

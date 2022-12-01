#!/usr/bin/env janet

(import argparse :prefix "")
(import ./day1/day1)

(def argparse-params
  ["Advent of Code 2022"
   "day" {:kind :option
          :short "d"
          :help "An API key for getting stuff from a server."
          :required true}])

(defmacro with-challenges [day day-int]
  (let [day-dirs (filter (fn [f] (peg/find "day" f)) (os/dir "."))
        day-main-fns (mapcat (fn [dir]
                               (let [[dir-day] (peg/match ~(* (to :d) (<- (some :d))) dir)]
                                 ~(,(int/u64 dir-day) (,(symbol dir "/main"))))) day-dirs)]
    ~(case ,day-int
       ,;day-main-fns
       (print "not a valid day: " ,day))))

(defn main [& args]
  (let [@{"day" day} (argparse ;argparse-params)
        day-int (try (int/u64 day) ([_]))]
    (unless day
      (os/exit 1))
    (with-challenges day day-int)))

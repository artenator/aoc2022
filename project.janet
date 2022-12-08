(declare-project
  :name "aoc2022"
  :description "Advent of Code 2022"
  :dependencies ["https://github.com/janet-lang/argparse.git"
                 "https://github.com/crocket/janet-utf8"])

(declare-executable
  :name "aoc2022"
  :entry "aoc.janet")

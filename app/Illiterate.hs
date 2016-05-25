module Main where


import Sites.Literate


main :: IO ()
main = interact (illiterate "clojure" clojureComments)


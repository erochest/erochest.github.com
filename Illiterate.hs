module Main where


import Sites.Literate


main :: IO ()
main = interact (snd . illiterate "clojure" clojureComments)


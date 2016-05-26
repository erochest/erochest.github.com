module Sites.Actions.Illiterate where


import Sites.Literate


illiterateClojure :: IO ()
illiterateClojure = interact (illiterate "clojure" clojureComments)

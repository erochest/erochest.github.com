

module Sites.Actions.DotEnv where


import Sites.Types


makeDotEnv :: ShellType -> IO ()
makeDotEnv = mapM_ putStrLn . dotEnv

dotEnv :: ShellType -> [String]
dotEnv Bash =
  [ ""
  , "export ANALYTICS_CODE="
  , "export PATH=$PATH:$(stack path --local-install-root)/bin"
  , ""
  , "rvm use $(< .ruby-version)@$(< .ruby-gemset)"
  , ""
  ]
dotEnv Fish =
  [ ""
  , "set -x ANALYTICS_CODE ???"
  , "set -xg PATH $PATH (stack path --local-install-root)/bin"
  , ""
  , "rvm use (cat .ruby-version)@(cat .ruby-gemset)"
  , ""
  ]


module Opts where


import qualified Data.Text           as T
import           Options.Applicative

import           Types


textOpt :: ReadM T.Text
textOpt = T.pack <$> str

buildOpts :: Parser Actions
buildOpts =   Build
          <$> many (argument textOpt (  metavar "HAKYLL_OPT"
                                     <> help "An option to pass to Hakyll."
                                     ))

opts' :: Parser Actions
opts' = subparser
    (  command "build" (info (helper <*> buildOpts)
                        (progDesc "Build the site."))
    )

opts :: ParserInfo Actions
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Generate my site and other helpers."
            <> header "errsite"
            )

parseOpts :: IO Actions
parseOpts = execParser opts

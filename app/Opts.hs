module Opts where


import qualified Data.Text           as T
import           Options.Applicative

import           Types


textOpt :: ReadM T.Text
textOpt = T.pack <$> str

hakyllOpts :: Parser Actions
hakyllOpts =   Hakyll
           <$> many (argument textOpt (  metavar "HAKYLL_OPT"
                                      <> help "An option to pass to Hakyll."
                                      ))

deployOpts :: Parser Actions
deployOpts
    =   Deploy
    <$> switch (  short 's'
               <> long "scratch"
               <> help "If given, this cleans the project and \
                       \rebuilds everything from scratch. (Default \
                       \is false.)")
    <*> switch (  short 'b'
               <> long "bail"
               <>  help "If given, this will bail before actually \
                        \deploying the site.")

illiterateOpts :: Parser Actions
illiterateOpts = pure Illiterate

opts' :: Parser Actions
opts' = subparser
    (  command "hakyll"  (info (helper <*> hakyllOpts)
                          (progDesc "Call Hakyll on the site."))
    <> command "deploy" (info (helper <*> deployOpts)
                         (progDesc "Deploy site to github pages."))
    <> command "illiterate" (info (helper <*> illiterateOpts)
                             (progDesc "Generate a literate Clojure file."))
    )

opts :: ParserInfo Actions
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Generate my site and other helpers."
            <> header "errsite"
            )

parseOpts :: IO Actions
parseOpts = execParser opts

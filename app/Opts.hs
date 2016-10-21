module Opts where


import           Data.Hashable
import qualified Data.HashSet        as S
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

manySet :: (Eq a, Hashable a, Alternative f) => f a -> f (S.HashSet a)
manySet = fmap S.fromList . many

draftOpts :: Parser Actions
draftOpts
    =   Draft
    <$> strOption (  short 'c' <> long "category" <> metavar "CATEGORY"
                  <> help "The slug for the category to put the post into.")
    <*> manySet (option textOpt
                    (  short 'T' <> long "tag" <> metavar "TAG"
                    <> help "A tag to include in the template."))
    <*> option textOpt (  short 't' <> long "title" <> metavar "TITLE"
                       <> help "The post's title.")
    <*> optional (strOption (  short 's' <> long "slug" <> metavar "SLUG"
                            <> help "A slug for the post (used in the URL)."))

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
    (  command "hakyll" (info (helper <*> hakyllOpts)
                          (progDesc "Call Hakyll on the site."))
    <> command "draft" (info (helper <*> draftOpts)
                        (progDesc "Stub out a new draft, including creating a\
                                  \ branch."))
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

{-# LANGUAGE OverloadedStrings #-}

module Sites.Literate
    ( CommentSpec(..)
    , CommentSpecList
    , MetadataList
    , illiterate
    , illiterateHtml
    , markdown
    , clojure
    , clojureComments
    , addMetadataList
    , metadataListContext
    ) where


import           Control.Arrow ((&&&), first, second)
import qualified Data.List as L
import           Data.Maybe
import           Data.Monoid
import           Hakyll
import           Sites.Base
import           Text.Pandoc


data CommentSpec = CommentLine String
                 | CommentBlock String String String
                 deriving (Show)

type CommentSpecList = [CommentSpec]
type MetadataList    = [(String, String)]

-- | Idle thought: I wonder if breaking this into two levels of types would
-- simplify or complicate the combination rules (`mappend`) below. My intuition
-- is that it would simplify the rules, but complicate using them, so I'd
-- prefer to keep it the way it is (except that breaking it into level would
-- let the compiler help keep things straight in a way it cannot do here). But
-- this is still something to try later.
data RunBlock = LCodeBlock ![String]
              | LTextBlock ![String]
              | MDBlock    !MetadataList
              | BlockRun   ![RunBlock]
              deriving (Show)

instance Monoid RunBlock where
        mempty = BlockRun []

        mappend a (BlockRun []) = a
        mappend (BlockRun []) b = b
        mappend (MDBlock a)    (MDBlock b)    = MDBlock $ a ++ b
        mappend (LCodeBlock a) (LCodeBlock b) = LCodeBlock $ a ++ b
        mappend (LTextBlock a) (LTextBlock b) = LTextBlock $ a ++ b
        mappend (MDBlock a)    (BlockRun (MDBlock b:br))    = BlockRun $ MDBlock    (a ++ b) : br
        mappend (LCodeBlock a) (BlockRun (LCodeBlock b:br)) = BlockRun $ LCodeBlock (a ++ b) : br
        mappend (LTextBlock a) (BlockRun (LTextBlock b:br)) = BlockRun $ LTextBlock (a ++ b) : br
        mappend a@(MDBlock _)    (BlockRun br) = BlockRun (a:br)
        mappend a@(LCodeBlock _) (BlockRun br) = BlockRun (a:br)
        mappend a@(LTextBlock _) (BlockRun br) = BlockRun (a:br)
        mappend (BlockRun br) (MDBlock b) =
            case last br of
                MDBlock a -> BlockRun $ init br ++ [MDBlock (a ++ b)]
                b         -> BlockRun $ br ++ [b]
        mappend (BlockRun br) (LCodeBlock b) =
            case last br of
                LCodeBlock a -> BlockRun $ init br ++ [LCodeBlock (a ++ b)]
                b            -> BlockRun $ br ++ [b]
        mappend (BlockRun br) (LTextBlock b) =
            case last br of
                LTextBlock a -> BlockRun $ init br ++ [LTextBlock (a ++ b)]
                b            -> BlockRun $ br ++ [b]
        mappend (BlockRun a) b@(BlockRun _) = foldr mappend b a
        mappend a b = BlockRun [a, b]


pullMetadata :: RunBlock -> MetadataList
pullMetadata (MDBlock md)   = md
pullMetadata (LTextBlock _) = []
pullMetadata (LCodeBlock _) = []
pullMetadata (BlockRun xs)  = concatMap pullMetadata xs

nl :: [String] -> [String]
nl = map (++ "\n")

renderBlock :: String -> RunBlock -> String
renderBlock _         (MDBlock _)    = ""
renderBlock _         (LTextBlock x) = concat $ nl x
renderBlock codeClass (LCodeBlock x) = concat . nl $ (("```" ++ codeClass):x) ++ ["```"]
renderBlock codeClass (BlockRun xs)  = L.intercalate "\n" $ map (renderBlock codeClass) xs

isEmpty :: RunBlock -> Bool
isEmpty (LCodeBlock cs) | not (any (not . L.null) cs) = True
isEmpty (LTextBlock []) = True
isEmpty (BlockRun   []) = True
isEmpty (BlockRun   br) = L.all isEmpty br
isEmpty _               = False

removeEmpty :: [RunBlock] -> [RunBlock]
removeEmpty = filter (not . isEmpty)

illiterate :: String -> CommentSpecList -> String -> (MetadataList, String)
illiterate codeClass specList = (pullMetadata &&& renderBlock codeClass)
                              . foldr mappend mempty
                              . removeEmpty
                              . snd
                              . L.mapAccumL (illiterateLine specList) Start
                              . lines

data CommentState = Start
                  | InMetadata
                  | CommentText String String
                  | ActiveText

stripStart :: Eq a => [a] -> [a] -> [a]
stripStart [] xs = xs
stripStart p xs  = fromMaybe xs $ L.stripPrefix p xs

stripEnd :: Eq a => [a] -> [a] -> [a]
stripEnd [] ys                    = ys
stripEnd s ys@(y:ys') | s == ys   = []
                      | otherwise = y : stripEnd s ys'
stripEnd s []                     = []

illiterateLine :: CommentSpecList
               -> CommentState
               -> String
               -> (CommentState, RunBlock)
illiterateLine _ Start ('#':'!':_) = (Start, mempty)

illiterateLine [] Start               line = (ActiveText,  LCodeBlock [line])
illiterateLine [] InMetadata          line = (ActiveText,  MDBlock    [])
illiterateLine [] ActiveText          line = (ActiveText,  LCodeBlock [line])
illiterateLine [] c@(CommentText _ _) line = (c,           LTextBlock [line])

illiterateLine _ c@(CommentText end strip) line
    | end `L.isSuffixOf` line = (ActiveText, LTextBlock [stripEnd end line])
    | otherwise               = (ActiveText, LTextBlock [stripStart strip line])

illiterateLine (CommentLine cs:css) Start line
    | line == cs ++ "---"    = (InMetadata, mempty)
    | cs `L.isPrefixOf` line = (ActiveText, LTextBlock [stripStart cs line])
    | otherwise              = illiterateLine css Start line
illiterateLine (CommentBlock start end strip:css) Start line
    | start `L.isPrefixOf` line = (CommentText end strip, LTextBlock [stripStart start line])
    | otherwise                 = illiterateLine css Start line

illiterateLine (CommentLine cs:css) InMetadata line
    | line == cs ++ "---"    = (ActiveText, mempty)
    | cs `L.isPrefixOf` line = (InMetadata, MDBlock [splitPair $ stripStart cs line])
    | otherwise              = illiterateLine css InMetadata line

illiterateLine (CommentLine cs:css) ActiveText line
    | cs `L.isPrefixOf` line = (ActiveText, LTextBlock [stripStart cs line])
    | otherwise              = illiterateLine css ActiveText line
illiterateLine (CommentBlock start end strip:css) ActiveText line
    | start `L.isPrefixOf` line = (CommentText end strip, LTextBlock [stripStart start line])
    | otherwise                 = illiterateLine css ActiveText line

splitPair :: String -> (String, String)
splitPair = first trim . second trim . break (== ':')

illiterateHtml :: String -> CommentSpecList -> String -> (MetadataList, String)
illiterateHtml codeClass specList = second markdown . illiterate codeClass specList

clojure :: String -> (MetadataList, String)
clojure = illiterateHtml "clojure" clojureComments

clojureComments :: CommentSpecList
clojureComments = [ CommentLine "; "
                  , CommentLine ";"
                  ]

markdown :: String -> String
markdown = debug
         . writeHtmlString defaultHakyllWriterOptions
         . readMarkdown defaultHakyllReaderOptions

addMetadataList :: MetadataList -> Context String -> Context String
addMetadataList = flip (foldr merge)
    where
        merge ("date", d) = mappend (dateContext d)
        merge (a, b)      = mappend (constField a b)

metadataListContext :: MetadataList -> Context String
metadataListContext = (`addMetadataList` mempty)


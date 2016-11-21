-- For those who can't read titles:
--
-- I've been interested in [literate programming](https://en.wikipedia.org/wiki/Literate_programming) for a while. I'm interested in the relationship between artificial languages and in how we can program more transparently. Literate programming sites at the margins of that.
--
-- I've also been playing around with [PureScript](http://www.purescript.org/) some. It's [Haskell](https://haskell-lang.org/), redesigned to remove some warts, that [transpiles](https://en.wikipedia.org/wiki/Source-to-source_compiler) to JavaScript. So far I haven't used it very much, but I'd like to work with it more.
--

-- So creating the ability to create literate PureScript posts on this blog has been on my todo list for a while. Now that I've actually started posting again, I thought I'd get this going. For those who are interested and maybe morbidly curious, the changes are in [this diff](https://github.com/erochest/erochest.github.com/compare/212d284670ae3355d5d40b45b11ebdb8c8a8feba...27bd636dbed225b0b803ac58cfc2e041b67943b5). There are a few moving parts, but mostly I just hand things over to [pulp](https://github.com/bodil/pulp) as much as I can.
--
-- Since this is a literate post, there are some formailities to get started:

module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Nullable (toMaybe)
import Data.Traversable (traverse)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument, htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.Document (createTextNode)
import DOM.Node.Node (appendChild)
import DOM.Node.NodeList (item)
import DOM.Node.ParentNode (querySelectorAll)
import DOM.Node.Types (textToNode)

-- (If this all seems a little verbose, I'm using a *very* low-level interface here. For anything more complicated, you'd want to use a higher-level library.)
--
-- There are still some things to do on this:
--
-- * Syntax color highlighting;
-- * The code doesn't work well with the margins;
-- * The code doesn't work well with the scalable font sizing.

main :: forall e. Eff (console :: CONSOLE, dom :: DOM | e) Unit
main = do
    doc      <-  window >>= document
    greeting <-  textToNode
             <$> createTextNode "Hello sailor!"
             (   htmlDocumentToDocument doc)
    querySelectorAll "#please-ignore .please-ignore"
        (   htmlDocumentToParentNode doc)
        >>= item 0
        >>= toMaybe
        >>> traverse (appendChild greeting)
    log "Hello sailor!"

-- As an added bonus, the scaffolded project created by [pulp](https://github.com/bodil/pulp) has a gratuitous [Zork](https://en.wikipedia.org/wiki/Zork) reference. How could I resist?

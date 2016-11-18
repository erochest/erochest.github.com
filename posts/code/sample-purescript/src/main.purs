---
title: Sample PureScript
date: 2016-11-17T09:42:23-0500
---

module Main where

-- This is a file containing some PureScript.

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

-- This is the `main` function. It's the entry-point for the script.

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"

-- Gratuitous Zork reference.

{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Actions
import           Opts


main :: IO ()
main = action =<< parseOpts

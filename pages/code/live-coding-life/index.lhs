---
title: Live-Coding Life
date: 2014-01-14T15:30:00Z
tags: haskell, automata
---

**So what's the point of this post? What do I want to talk about for it?**

The other week I took the opportunity to play with and to implement [Conway's
Life][life] for the umpteenth time.

(This post is written in [Literate Haskell][lhs], so you can copy and paste it
into a file with the `.lhs` extension and compile it with a Haskell compiler.
First we need to get some formalities out of the way.)

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Array.Repa hiding ((++), map)
import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as V
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Graphics.Gloss.Raster.Array
import           System.Random.MWC
\end{code}

Now we can concentrate on the types. The types should capture basic information
about the requirements of the program and about *invariants*, or things that
should always be true in the program.

This first type doesn't have anything to do with the program itself, but it's
interesting in its own right. I used the `Hole` type as I was developing for
what's been referred to as [*hold-driven development*][hdd].

\begin{code}
data Hole = Hole

newtype World a = World { getWorld :: Array U DIM2 a }

type LifeWorld = World Bool

data LifeCast = LifeCast
              { _running :: Bool
              , _world   :: LifeWorld
              }

-- Settings

worldScale :: Int
worldScale = 3

worldWidth :: Int
worldWidth = 500

worldHeight :: Int
worldHeight = 309

-- Lenses

world :: Lens' LifeCast LifeWorld
world f (LifeCast r w) = fmap (LifeCast r) (f w)

running :: Lens' LifeCast Bool
running f (LifeCast r w) = fmap (`LifeCast` w) (f r)

-- | Moore neighborhood

leftCol :: DIM2 -> DIM2 -> Int
leftCol (Z :. _ :. w') (Z :. _ :. w)
    | w == 0    = w' - 1
    | otherwise = w  - 1

rightCol :: DIM2 -> DIM2 -> Int
rightCol (Z :. _ :. w') (Z :. _ :. w)
    | w == (w' - 1) = 0
    | otherwise     = w + 1

upRow :: DIM2 -> DIM2 -> Int
upRow (Z :. h' :. _) (Z :. h :. _)
    | h == 0    = h' - 1
    | otherwise = h  - 1

downRow :: DIM2 -> DIM2 -> Int
downRow (Z :. h' :. _) (Z :. h :. _)
    | h == (h' - 1) = 0
    | otherwise     = h + 1

mooreNeighborhood :: DIM2 -> DIM2 -> [DIM2]
mooreNeighborhood extent pos =
    let [xp, yp] = listOfShape pos
        x1       = leftCol extent pos
        x2       = rightCol extent pos
        y1       = upRow extent pos
        y2       = downRow extent pos
    in  map (uncurry ix2) [ (y1, x1), (yp, x1), (y2, x1)
                          , (y1, xp),           (y2, xp)
                          , (y1, x2), (yp, x2), (y2, x2)
                          ]

-- | Display and colors

cellColor :: Bool -> Color
cellColor True  = dim red
cellColor False = light black

colorWorld :: LifeWorld -> Array D DIM2 Color
colorWorld = R.map cellColor . getWorld

-- | Generating the next generation

step :: LifeWorld -> IO LifeWorld
step w = fmap World . computeP . R.traverse (getWorld w) id $ \getter pos ->
      conway (getter pos). length . filter id . map getter
    $ mooreNeighborhood worldExtent pos
    where worldExtent = extent $ getWorld w

stepCast :: LifeCast -> IO LifeCast
stepCast lc@(LifeCast True  w) = LifeCast True <$> step w
stepCast lc@(LifeCast False _) = return lc

-- Rules
conway :: Bool -> Int -> Bool
conway current 2 = current
conway _       3 = True
conway _       _ = False

randomWorld :: Int -> Int -> IO LifeWorld
randomWorld w h = withSystemRandom . asGenIO $ \gen ->
        World . fromUnboxed (ix2 h w)
    <$> (uniformVector gen (w * h) :: IO (V.Vector Bool))

onEvent :: Event -> LifeCast -> IO LifeCast
onEvent (EventKey (SpecialKey KeySpace) Up _ _) = return . over running not
onEvent _                                       = return

main :: IO ()
main = do
    initial <- LifeCast False <$> randomWorld worldWidth worldHeight
    playArrayIO (InWindow "Life" (tow worldWidth, tow worldHeight) (0, 0))
                (worldScale, worldScale)
                7
                initial
                (return . colorWorld . _world)
                onEvent
                (const stepCast)
    where tow = (*worldScale)
\end{code}

[asciinema]: http://asciinema.org/
[consolecast]: http://asciinema.org/a/7127
[hdd]: http://matthew.brecknell.net/post/hole-driven-haskell/
[lhs]: http://www.haskell.org/haskellwiki/Literate_programming
[life]: http://en.wikipedia.org/wiki/Conway%27s_life
[type-holes]: http://www.haskell.org/haskellwiki/GHC/TypeHoles


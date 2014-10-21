module Shuffle
 ( shuffle )
   where

import Data.Array ((!!), deleteAt, length)
import Data.Maybe 
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Math (floor)

-- Unsafe!
fromJust :: forall a. Maybe a -> a
fromJust (Just a) = a

shuffleAccordingTo' :: forall a. [Number] -> [a] -> [a] -> [a]
shuffleAccordingTo' [] [] result = result
shuffleAccordingTo' (i:is) input result =
  shuffleAccordingTo' is splicedInput (elem:result)
    where
      -- This is just for internal use, so if this `fromJust` fails there's a bug in this file
      elem = fromJust (input !! i)
      splicedInput = deleteAt i 1 input

-- `shuffling` should be an element of [n-1] \times [n-2] \times ... \times [0]
shuffleAccordingTo :: forall a. [Number] -> [a] -> [a]
shuffleAccordingTo shuffling toBeShuffled = shuffleAccordingTo' shuffling toBeShuffled []

-- This should be made to be tail-recursive
getShuffleOrder :: forall eff. Number -> Eff (random :: Random | eff) [Number]
getShuffleOrder 0 = return []
getShuffleOrder n = do
  r <- random
  let i = floor (r * n)
  rest <- getShuffleOrder (n - 1)
  return $ i:rest

shuffle :: forall a eff. [a] -> Eff (random :: Random | eff) [a]
shuffle lst = do
  order <- getShuffleOrder $ length lst
  return $ shuffleAccordingTo order lst 

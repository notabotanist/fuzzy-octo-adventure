module Data.Vect.Float.Hyper where

import Data.Vect

data Vec5 = Vec5 !Float !Float !Float !Float !Float
  deriving (Show, Eq)

-- |The components are row vectors
data Mat5 = Mat5 !Vec5 !Vec5 !Vec5 !Vec5 !Vec5
  deriving (Show, Eq)

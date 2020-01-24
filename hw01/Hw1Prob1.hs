module Hw1Prob1 where

type Text = Int
type Key  = Int

s :: Rational
s = sum [1..26]

texts :: [Text]
texts = [0..25]

textProb :: Text -> Rational
textProb = (/ s) . (+ 1) . toRational

keys :: [Key]
keys = [0..25]

keyProb :: Key -> Rational
keyProb = (/ s) . (+ 1) . toRational

enc :: Text -> Key -> Text
enc m k = mod (m + k) 26


-- ========================================================
-- == SOLUTIONS ===========================================
-- ========================================================

-- Pr[C = 0]
partA :: Rational
partA = sum [ (textProb m) * (keyProb k) | k <- keys,
                                           m <- texts,
                                           enc m k == 0 ]


module Hw2Prob2 where

import Vigenere ( Key
                , keys1
                , keys2
                , keys3
                )

{- Different (slower) way of deriving key probabilities

keys1Dist :: [(Key, Rational)]
keys1Dist = [(k, 1 / 26^1) | k <- keys1]

keys2Dist :: [(Key, Rational)]
keys2Dist = [(k, 1 / 26^2) | k <- keys2]

keys3Dist :: [(Key, Rational)]
keys3Dist = [(k, 1 / 26^3) | k <- keys3]

keysDist :: [(Key, Rational)]
keysDist = concatMap (map normalize) [ keys1Dist
                                     , keys2Dist
                                     , keys3Dist ]
  where normalize (k, p) = (k, p / 3)

prob :: Key -> Rational
prob k = sum . map snd $ filter ((== k) . fst) keysDist

-}

prob :: Key -> Rational
prob k = (/3) . recip . (26^) . length $ k


-- ========================================================
-- == SOLUTIONS ===========================================
-- ========================================================

-- Pr[K = a]
partA :: Rational
partA = prob [0..0]

-- Pr[K = ab]
partB :: Rational
partB = prob [0..1]

-- Pr[K = abc]
partC :: Rational
partC = prob [0..2]


module Hw2Prob1 where

import Shift ( Text
             , texts
             , Key
             , keys
             , enc
             )

textsDist :: [(Text, Rational)]
textsDist = map (\m -> (m, prob m)) texts
  where prob = (/ 10) . (+ 1) . toRational

keysDist :: [(Key, Rational)]
keysDist = zip keys $ cycle [1 / 26]

probEnc :: Text -> Text -> Key -> Rational
probEnc c m k = sum . map snd . filter match $ enc m k
  where match = (== c) . fst

probEncK :: Text -> Rational
probEncK m = sum [p * (probEnc 10 m k) | (k, p) <- keysDist]


-- ========================================================
-- == SOLUTIONS ===========================================
-- ========================================================

-- Pr[Enc(m, K) = 10] (same for all m)
partA :: Rational
partA = probEncK 0

-- Pr[Enc(M, K) = 10]
partB :: Rational
partB = sum [p * (probEncK m) | (m, p) <- textsDist]


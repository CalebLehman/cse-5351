module Hw2Prob4 where

import Vigenere ( Text
                , Key
                , keys
                , enc
                )

prob :: Key -> Rational
prob k = (/3) . recip . (26^) . length $ k

-- A(m0, m1, Enc(mb, k))
-- adversary c = False means A guesses m0
-- adversary c = True  means A guesses m1
adversary :: Text -> Bool
adversary (x:(y:zs)) | x == y = False
adversary _                   = True

m0 :: Text
m0 = [0, 0, 1]

m1 :: Text
m1 = [0, 1, 1]


-- ========================================================
-- == SOLUTIONS ===========================================
-- ========================================================

-- Pr[PrivK(m0, m1, A) = 1]
partA :: Rational
partA = (/2) . sum . map prob $ success0 ++ success1
  where success  = success0 ++ success1
        success0 = filter (not . adversary . enc m0) keys
        success1 = filter (adversary . enc m1) keys


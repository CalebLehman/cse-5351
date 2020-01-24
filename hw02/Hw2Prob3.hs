module Hw2Prob3 where

import Vigenere ( Text
                , Key
                , keys
                , enc
                )

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

-- { k | A(m0, m1, Enc(m0, k)) = 0 }
partA :: [Key]
partA = filter (not . adversary . enc m0) keys

-- { k | A(m0, m1, Enc(m1, k)) = 1 }
partB :: [Key]
partB = filter (adversary . enc m1) keys


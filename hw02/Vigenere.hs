module Vigenere ( Text
                , texts
                , Key
                , keys1
                , keys2
                , keys3
                , keys
                , enc
                , dec
                ) where

type Text = [Int]
type Key  = [Int]

texts :: [Text]
texts = [[a, b, c] | a <- [0..25],
                     b <- [0..25],
                     c <- [0..25] ]

keys1 :: [Key]
keys1 = [[a] | a <- [0..25]]

keys2 :: [Key]
keys2 = [[a, b] | a <- [0..25],
                  b <- [0..25] ]

keys3 :: [Key]
keys3 = [[a, b, c] | a <- [0..25],
                     b <- [0..25],
                     c <- [0..25] ]

keys :: [Key]
keys = keys1 ++ keys2 ++ keys3

enc :: Text -> Key -> Text
enc = (. cycle) . zipWith encrypt
  where encrypt m k = mod (m + k) 26

dec :: Text -> Key -> Text
dec = (. cycle) . zipWith decrypt
  where decrypt c k = mod (c - k) 26


module Shift ( Text
             , texts
             , Key
             , keys
             , enc
             , dec
             ) where

type Text = Int
type Key  = Int

texts :: [Text]
texts = [0..3]

keys :: [Key]
keys = [0..25]

enc :: Text -> Key -> [(Text, Rational)]
enc m k = [(encrypt m k, 1 / 2) | encrypt <- [encrypt1, encrypt2]]
  where encrypt1 m k = mod (m + k) 26
        encrypt2 m k = mod (m + k + 5) 26

dec :: Text -> Key -> Maybe Text
dec c k
  | elem m1 texts = Just m1
  | elem m2 texts = Just m2
  | otherwise     = Nothing
  where m1 = mod (c - k) 26
        m2 = mod (c - k - 5) 26


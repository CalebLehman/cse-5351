module Hw1Prob3 where

type Text = [Int]
type Key  = [Int]

m0 :: Text
m0 = [0, 1, 2, 3]

m1 :: Text
m1 = [1, 4, 3, 6]

enc :: Text -> Key -> Text
enc = (. cycle) . zipWith encrypt
  where encrypt m k = mod (m + k) 26

crack3 :: Text -> Maybe Text
crack3 (x:(y:(z:(w:[]))))
  | w - x == 3 = Just m0
  | w - x == 5 = Just m1
crack3 _ = Nothing


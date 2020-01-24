module Hw1Prob2 where

type Text = [Int]
type Key  = Int

m0 :: Text
m0 = [0, 1, 2, 3]

m1 :: Text
m1 = [1, 4, 3, 6]

enc :: Text -> Key -> Text
enc m k = map encrypt m
  where encrypt = (`mod` 26) . (+ k)

crack :: Text -> Maybe Text
crack (x:(y:zs))
  | y - x == 1 = Just m0
  | y - x == 3 = Just m1
crack _ = Nothing


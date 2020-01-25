module ShiftCypher ( enc, dec, solve ) where

import Data.Char
import Data.List
import Data.Ord

-- CYPHER IMPLEMENTATION
-- An implementation of the basic SHIFT CYPHER

shift :: Int -> Char -> Char
shift k = chr . (+ 97) . (`mod` 26) . (+ k) . (subtract 97) . ord

enc :: Int -> String -> String
enc k = map (shift k)

dec :: Int -> String -> String
dec k = map (shift (negate k))

-- BREAKER IMPLEMENTATION
-- An imlpementation of a strategy to break the shift cypher when the
-- encoded language is English.
-- Assumes the message has had non-alphabetic characters stipped and has
-- been converted to lower-case.

englishFreqs :: [Float]
englishFreqs = [ 08.2 -- a
               , 01.5 -- b
               , 02.8 -- c
               , 04.3 -- d
               , 12.7 -- e
               , 02.2 -- f
               , 02.0 -- g
               , 06.1 -- h
               , 07.0 -- i
               , 00.2 -- j
               , 00.8 -- k
               , 04.0 -- l
               , 02.4 -- m
               , 06.7 -- n
               , 01.5 -- o
               , 01.9 -- p
               , 00.1 -- q
               , 06.0 -- r
               , 06.3 -- s
               , 09.1 -- t
               , 02.8 -- u
               , 01.0 -- v
               , 02.4 -- w
               , 00.2 -- x
               , 02.0 -- y
               , 00.1 -- z
               ]

computeFreq :: String -> [Float]
computeFreq s = map (normalize . counter) ['a'..'z']
  where counter c = length . filter (== c) $ s
        normalize = (/ n) . fromIntegral
        n         = fromIntegral $ length s

score :: [Float] -> Float
score = sum . zipWith (*) englishFreqs

getShift :: String -> Int
getShift s = fst . maximumBy (comparing snd) . zip [0..] $ scores
  where scores = [score . take 26 . drop k . cycle $ freqs | k <- [0..25]]
        freqs  = computeFreq s

solve :: String -> String
solve s = dec (getShift s) s

declaration :: String
declaration = filter (`elem` ['a'..'z']) . map toLower $ "When in the Course of human events, it becomes necessary for one people to dissolve the political bands which have connected them with another, and to assume, among the Powers of the earth, the separate and equal station to which the Laws of Nature and of Nature's God entitle them, a decent respect to the opinions of mankind requires that they should declare the causes which impel them to the separation"

module StringAlignment where
import Data.List
import Data.Char

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

score :: Char -> Char -> Int
score '-' y = scoreSpace
score x '-' = scoreSpace
score x y
  | x == y = scoreMatch
  | otherwise = scoreMismatch


similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore s [] = (*) scoreMismatch $ length s
similarityScore [] s = (*) scoreMismatch $ length s
similarityScore (x:xs) (y:ys) =
  maximum
  [ (similarityScore xs     ys    ) + (score x   y)
  , (similarityScore (x:xs) ys    ) + (score '-' y)
  , (similarityScore xs     (y:ys)) + (score x '-') ]


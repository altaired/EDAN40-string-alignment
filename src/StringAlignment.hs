module StringAlignment where
import Data.List
import Data.Char

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1


-- 2 a)
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


-- 2 b)
-- For each tuple containing lists L1 and L2, prepend h1 and h2 to L1 and L2 respectivly
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]


-- 2 c)
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = filter (\e -> m == (valueFcn e)) xs
  where m = maximum (map valueFcn xs)


-- 2 d)
type AlignmentType = (String,String)

score2 :: (String, String) -> Int
score2 ([], []) = 0
score2 ((x:xs), (y:ys)) = (score x y) + score2 (xs, ys)


optAlignments :: String -> String -> [AlignmentType]
optAlignments s1 s2 = maximaBy score2 (allAlignments s1 s2)


allAlignments :: String -> String -> [AlignmentType]
allAlignments [] [] = []
allAlignments s [] = [(s, replicate (length s) '-')]
allAlignments [] s = [(replicate (length s) '-', s)]
allAlignments (x:xs) (y:ys) =
  attachHeads x y $ concat
  [ (allAlignments xs       ys  )
  , (allAlignments xs   ('-':ys))
  , (allAlignments ('-':xs) ys  ) ]


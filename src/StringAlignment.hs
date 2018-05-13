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
-- TODO: Optimize
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = filter (\e -> m == (valueFcn e)) xs
  where m = maximum (map valueFcn xs)


-- 2 d)
type AlignmentType = (String,String)

score2 :: AlignmentType -> Int
score2 ([], []) = 0
score2 ((x:xs), (y:ys)) = (score x y) + score2 (xs, ys)


optAlignments :: String -> String -> [AlignmentType]
optAlignments s1 s2 = maximaBy score2 (allAlignments s1 s2)


allAlignments :: String -> String -> [AlignmentType]
allAlignments [] [] = []
allAlignments s [] = [(s, replicate (length s) '-')]
allAlignments [] s = [(replicate (length s) '-', s)]
allAlignments (x:xs) (y:ys) =
  concat
  [ attachHeads x y (allAlignments xs       ys  )
  , attachHeads x '-' (allAlignments xs   (y:ys))
  , attachHeads '-' y (allAlignments (x:xs) ys  ) ]


attachTails :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachTails tx ty aList = [(xs ++ [tx], ys ++ [ty]) | (xs,ys) <- aList]

type Entry = (Int, [AlignmentType])

optimalOptAlignments :: String -> String -> [AlignmentType]
optimalOptAlignments xs ys = snd $ alignment (length xs) (length ys)
  where
    alignment :: Int -> Int -> Entry
    alignment i j = table!!i!!j
    table = [[ entry i j | j<-[0..]] | i<-[0..]]
    entry :: Int -> Int -> Entry
    entry 0 0 = (0, [])
    entry i 0 = (i * scoreSpace, [(take i xs, replicate i '-')])
    entry 0 j = (j * scoreSpace, [(replicate j '-', take j ys)]) -- TODO: Store only Int
    entry i j = ((fst . head) tuples, concat [b | (_, b) <- tuples])
      where
        tuples = maximaBy fst
          [ merge (alignment (i-1) (j-1))  x  y
          , merge (alignment  i    (j-1)) '-' y
          , merge (alignment (i-1)  j   )  x '-' ]
        merge :: Entry -> Char -> Char -> Entry
        merge (s, als) x y = (s + (score x y), attachTails x y als)
        x = xs!!(i - 1)
        y = ys!!(j - 1)


outputAlignments :: String -> String -> IO()
outputAlignments s1 s2 = do
  putStrLn $ unlines $ concatMap (\(a,b) -> ["", a, b, ""]) alg
  putStrLn $ show $ length alg
  where alg = optimalOptAlignments s1 s2


outputAlignments2 :: String -> String -> IO()
outputAlignments2 s1 s2 = do
  putStrLn $ unlines $ concatMap (\(a,b) -> ["", a, b, ""]) alg
  putStrLn $ show $ length alg
  where alg = optAlignments s1 s2



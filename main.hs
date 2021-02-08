import Data.List
import Data.Maybe
import System.Random

string = "The quick brown fox jumps hover the lazy dog"
ngramSize = 3

-- Replace a list element
replace pos val list = take pos list
    ++ val : drop (pos+1) list

-- Generate the ngrams and their chars
genNgrams = map
    (\x ->
        [ take ngramSize $ drop x string
        , [string !! max 0 (x + ngramSize)]
    ])
    [0..(length string - ngramSize - 1)]

-- Generate the non-repeated ngrams
ngrams :: [[Char]]
ngrams =
    foldl (\arr x -> if x `elem` arr
        then arr
        else arr ++ [x] ) [] $
    map head genNgrams

-- Generate the non-repeated characters
characters =
    foldl (\arr x -> if x `elem` arr
        then arr
        else arr ++ [x] ) [] $
    map (!! 1) genNgrams

-- Like foldl but with index
foldli f z [] i     = z
foldli f z (x:xs) i = foldli f (f z x i) xs (i+1)

chances = foldli
  (\arr x i -> replace
    (fromJust $ elemIndex x ngrams)
    (arr !! fromJust (elemIndex x ngrams)
      ++ [(genNgrams !! i) !! 1])
    arr)
  (replicate (length genNgrams - 1) [])
  (map head genNgrams)
  0

foldIndex :: ([Char] -> [Char]) -> [Char] -> Int -> [Char]
foldIndex f t 0 = t
foldIndex f t i = if (f t == t)
  then t
  else foldIndex f (f t) (i - 1)

lastNgram x = drop (length x - ngramSize) x
randomElement l g = l !! fst (randomR (0, length l - 1) g)

getNext :: [Char] -> Maybe Int -> StdGen -> [Char]
getNext last i g
  | fromMaybe (-1) i == -1 = []
  | otherwise = randomElement (chances !! fromJust i) g

genOutput g = foldIndex (\x -> x ++ getNext (lastNgram x) (elemIndex (lastNgram x) ngrams) g)

output = "The "

main = do
  g <- newStdGen
  print $ genOutput g output 100

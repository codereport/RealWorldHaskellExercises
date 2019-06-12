import Data.Maybe
import Data.Char

-- Exercise 1
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [x]    = Just x
safeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

-- Exercise 2
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs = (a ++ h) : splitWith p t
  where (a, b) = span p xs
        (h, t) | null b  = ([], [])
               | otherwise = ([head b], tail b)

-- length with foldl
len :: [a] -> Int
len = foldl (\a b -> a + 1) 0

-- Exercise 1 & 2
asInt_fold :: String -> Int
asInt_fold s@(sign:num)
    | sign == '-' = -1 * stoi num
    | otherwise   = stoi s
        where stoi s = foldl (\a b -> a * 10 + digitToInt b) 0 s

-- Exercise 3
-- Technically fails a couple case
--  1. Nondigit first character that isn't -
--  2. Values of String == to maxBount length but still greater
asInt_fold2 :: String -> Int
asInt_fold2 s@(sign:num)
    | null s      = 0 -- this fails?
    | s == "-"    = 0
    | nondigit    = error "Not all characters are digits"
    | overflow    = error "String value is to large"
    | sign == '-' = -1 * stoi num
    | otherwise   = stoi s
        where stoi s = foldl (\a b -> a * 10 + digitToInt b) 0 s
              nondigit = any (not . isDigit) num
              overflow = length s > length (show (maxBound :: Int))

-- Exercise 4
type ErrorMessage = String

asInt_fold3 :: String -> Either ErrorMessage Int
asInt_fold3 s@(sign:num)
    | null s      = Right 0
    | s == "-"    = Right 0
    | nondigit    = Left "Not all characters are digits"
    | overflow    = Left "String value is to large"
    | sign == '-' = Right (-1 * stoi num)
    | otherwise   = Right (stoi s)
        where stoi s = foldl (\a b -> a * 10 + digitToInt b) 0 s
              nondigit = any (not . isDigit) num
              overflow = length s > length (show (maxBound :: Int))

-- Exercise 5 & 6
concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- Exercsie 7
takewhile' :: (a -> Bool) -> [a] -> [a]
takewhile' _ [] = []
takewhile' p (x:xs)
    | p x = x : takewhile' p xs
    | otherwise = []

takewhile'' :: (a -> Bool) -> [a] -> [a]
takewhile'' p = foldr (\b a -> if p b then b : a else []) []

-- SIDE-BY-SIDE COMPARISON
--

takewhile_foldl :: (a -> Bool) -> [a] -> [a]
takewhile_foldl p = foldl (\a b -> if p b then a ++ [b] else a) []

takewhile_foldr :: (a -> Bool) -> [a] -> [a]
takewhile_foldr p = foldr (\b a -> if p b then b : a else []) []

---
---

-- Exercise 8 & 9
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' op = foldr step [[]]
    where step e ([]:t) = [e] : t
          step e acc 
            | op e (head (head acc)) = (e : head acc) : (tail acc)
            | otherwise = [e] : acc

-- Exercise 10
any' :: (a -> Bool) -> [a] -> Bool
any' p = foldl (\a b -> a || p b) False

-- cycle not possible

words' :: String -> [String]
words' = foldr (\b a -> if b == ' ' 
                        then [[]] ++ a 
                        else (b : head a) : tail a) [[]]

main :: IO ()
main = do
    print $ safeHead ([] :: [Int])
    print $ safeTail ([] :: [Int])
    print $ safeLast ([] :: [Int])
    print $ safeInit ([] :: [Int])
    print $ splitWith odd [1..10]
    print $ len [1..10]
    print $ (asInt_fold "1234") * 10
    print $ asInt_fold "101"
    print $ asInt_fold "-31337"
    print $ asInt_fold "1798"
    -- print $ asInt_fold2 "" -- can't get this to work
    print $ asInt_fold2 "-"
    --print $ asInt_fold2 "2.7"
    --print $ asInt_fold2 "314159265358979323846"
    -- print $ asInt_fold2 "?123" -- this fails
    print $ concat' [[1],[2],[3]]
    print $ takewhile' (<5) [1..10]
    print $ takewhile' (<5) []
    print $ takewhile'' (<5) [1..10]
    print $ takewhile'' (<5) []
    print $ takewhile_foldl (<5) [1..10]
    print $ takewhile_foldr (<5) [1..10]
    print $ groupBy' (==) [1,1,2,3,3,4,4]
    print $ any' odd [1,2,3]
    print $ any' odd [2,4,6]
    print $ words' "I am Thor, God of Asgard."

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

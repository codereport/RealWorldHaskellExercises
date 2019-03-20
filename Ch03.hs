
-- Algebraic Data Types
type Name = String
type Title = String

data Talk = Talk Name Title
    deriving (Show)

-- Exercise 1
data List a = Cons a (List a) | Nil
    deriving (Show)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x (xs)) = x : fromList xs

-- Exercise 2
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))

-- Exercise 1 & 2
len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

-- Exercise 3
avg :: Fractional a => [a] -> a
avg xs = (sum xs) / (fromIntegral (len xs))

-- Exercise 4 
createPalindrome :: [a] -> [a]
createPalindrome xs = xs ++ reverse xs

-- Exercise 5
verifyPalindrome :: Eq a => [a] -> Bool
verifyPalindrome []  = True
verifyPalindrome [_] = True
verifyPalindrome (x:xs) 
    | x == last xs = verifyPalindrome (init xs)
    | otherwise    = False

main :: IO ()
main = do
    print $ Talk "Conor Hoekstra" "Algorithm Intuition"
    let l = Cons 3 (Cons 2 (Cons 1 (Cons 0 Nil)))
    print $ fromList l
    print $ len [1..10]
    print $ avg [1..10]
    print $ createPalindrome [1..5]
    print $ verifyPalindrome [ 1, 2, 2, 1]
    print $ verifyPalindrome [ 1, 2, 3, 2, 1]
    print $ verifyPalindrome [ 1, 2, 1, 1]

import Data.List

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
    -- Solution 1
verifyPalindrome :: Eq a => [a] -> Bool
verifyPalindrome []  = True
verifyPalindrome [_] = True
verifyPalindrome (x:xs) 
    | x == last xs = verifyPalindrome (init xs)
    | otherwise    = False

    -- Solution 2
verifyPalindrome2 :: Eq a => [a] -> Bool
verifyPalindrome2 xs = (take n xs) == reverse (drop (n + m) xs)
    where n = div (length xs) 2
          m = if odd (length xs) then 1 else 0

    -- Solution 3
verifyPalindrome3 :: Eq a => [a] -> Bool
verifyPalindrome3 xs = xs == reverse xs

-- Exercise 6
sortByLen :: [[a]] -> [[a]]
sortByLen xs = sortOn length xs 

-- Exercise 7 & 8
my_intersperse :: a -> [[a]] -> [a]
my_intersperse _ [s] = s
my_intersperse sep (x:xs) = x ++ [sep] ++ my_intersperse sep xs

-- Exercise 9
data MyTree a = Leaf a | MyNode (MyTree a) a (MyTree a)

treeHeight :: MyTree a -> Int
treeHeight (Leaf _) = 1
treeHeight (MyNode l _ r) = 1 + max (treeHeight l) (treeHeight r) 

-- Exercise 10
data LineType = RightTurn | LeftTurn | Straight
    deriving (Show, Eq)

-- Exercise 11
-- 
data Point2D = Point2D { x, y :: Double } 
    deriving (Show, Ord, Eq)

determineLineType :: Point2D -> Point2D -> Point2D -> LineType
determineLineType p1 p2 p3
    | determinant > 0 = LeftTurn
    | determinant < 0 = RightTurn
    | otherwise = Straight
        where determinant = (x p2 - x p1) * (y p3 - y p1) - (y p2 - y p1) * (x p3 - x p1)
-- https://math.stackexchange.com/questions/2121112/how-do-i-visualize-if-three-points-represent-a-right-or-left-turn

-- Exerxise 12
turns :: [Point2D] -> [LineType]
turns (a:b:c:d) = determineLineType a b c : turns (b:c:d)
turns _ = []

-- Exercise 13 (Did not do)
-- But you can find solution here: https://github.com/Vincibean/real-world-haskell/blob/develop/ch03/ex15.hs

main :: IO ()
main = do
    print $ Talk "Conor Hoekstra" "Algorithm Intution"
    let l = Cons 3 (Cons 2 (Cons 1 (Cons 0 Nil)))
    print $ fromList l
    print $ len [1..10]
    print $ avg [1..10]
    print $ createPalindrome [1..5]
    print $ verifyPalindrome3 [ 1, 2, 2, 1]
    print $ verifyPalindrome3 [ 1, 2, 3, 2, 1]
    print $ verifyPalindrome3 [ 1, 2, 1, 1]
    print $ sortByLen [ [1..3], [1..2], [1] ]
    print $ my_intersperse ' ' ["Hello", "World!"]
    print $ treeHeight (Leaf 1)
    print $ treeHeight (MyNode (Leaf 1) 2 (Leaf 1))
    print $ determineLineType (Point2D 0 0) (Point2D 0 1) (Point2D (-1) 1)
    print $ turns [ (Point2D 0 0), (Point2D 0 1), (Point2D (-1) 1), (Point2D 0 2) ]

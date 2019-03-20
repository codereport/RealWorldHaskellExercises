
lastButOne :: [a] -> a
lastButOne [x, _] = x
lastButOne (x:xs) = lastButOne xs

main :: IO ()
main = do
    let x = [1..5]
    print $ lastButOne x

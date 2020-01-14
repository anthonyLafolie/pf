alterne [] = []
alterne [x] = [x]
alterne (x:(xs:xss)) = x:alterne(xss)


combine :: (a -> b -> c) -> [a] -> [b] -> [c]
combine f [] _ = []
combine f _ [] = []
combine f (x:xs) (y:ys) = (f x y):combine f xs ys

pasPascal :: [Integer] -> [Integer]
pasPascal [] = [1]
pasPascal x = zipWith (+) (x ++ [0]) ([0] ++ x)

pascal :: [[Integer]]
pascal = iterate pasPascal [1]

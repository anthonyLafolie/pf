alterne [] = []
alterne [x] = [x]
alterne (x:(xs:xss)) = x:alterne(xss)


combine :: (a -> b -> c) -> [a] -> [b] -> [c]
combine f [] _ = []
combine f _ [] = []
combine f (x:xs) (y:ys) = (f x y):combine f xs ys

pasPascal :: [Integer] -> [Integer]
pasPascal (x:xs) = zipWith (+) ((x:xs)++[0]) (([0]++(x:xs))++[0])

pascal :: [[Integer]]
pascal = iterate pasPascal [1]

type Point = (Float, Float)
type Path  = [Point]

pointAintercaler :: Point -> Point -> Point
pointAintercaler (xa, ya) (xb, yb) = ((xa + xb)/2 + (yb - ya)/2, (ya + yb)/2 + (xa - xb)/2)

pasDragon :: Path -> Path
pasDragon [] = []
pasDragon (x:xs) = ((pointAintercaler x (head(xs)) ):pasDragon xs)
pasDragon (x:xs) = ((pointAintercaler x (head(xs)) ):pasDragon (xs)

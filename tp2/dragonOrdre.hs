import Graphics.Gloss

main = animate (InWindow "DragonOrdre" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))

dragonAnime a b t = Line (dragonOrdre a b (round t `mod` 20))


pointAintercaler :: Point -> Point -> Point
pointAintercaler (xa, ya) (xb, yb) = ((xa + xb)/2 + (yb - ya)/2, (ya + yb)/2 + (xa - xb)/2)

pasDragon :: Path -> Path
pasDragon [x] = [x]
pasDragon (x:[xs]) = (x:((pointAintercaler x xs):[xs]))
pasDragon (x:(xs:xss)) = [x] ++ [(pointAintercaler x xs)] ++ [xs] ++ [(pointAintercaler (head(xss)) xs)] ++ pasDragon xss

dragonOrdre :: Point -> Point -> Int -> Path
dragonOrdre x y 0 = [x] ++ [y]
dragonOrdre x y n = [x] ++ (dragonOrdre x (pointAintercaler x y) (n-1)) ++ [(pointAintercaler x y)] ++ (dragonOrdre y (pointAintercaler x y) (n-1) ) ++ [y]

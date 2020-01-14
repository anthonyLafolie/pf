import Graphics.Gloss


main = animate (InWindow "DragonOrdre" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))

dragonAnime a b t = Line (dragonOrdre a b (round t `mod` 20))


pointAintercaler :: Point -> Point -> Point
pointAintercaler (xa, ya) (xb, yb) = ((xa + xb)/2 + (yb - ya)/2, (ya + yb)/2 + (xa - xb)/2)

dragonOrdre :: Point -> Point -> Int -> Path
dragonOrdre x y 0 = [x] ++ [y]
dragonOrdre a b n = let c = pointAintercaler a b in  dragonOrdre a c (n-1) ++ reverse(init(dragonOrdre b c (n-1)))

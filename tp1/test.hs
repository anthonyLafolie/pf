--Q3
sommeDeXaY x y = sum[x..y]

--Q4
{- Le type ici n'est pas celui attendu -}

somme :: [Int] -> Int
somme [] = 0
somme (x:xs) = x + (somme xs)


--Q5
last' xs = head (reverse xs)

init'[x] = []
init' xs = take(length(xs)-1) xs

init'' xs = reverse(drop 1(reverse xs))

--Q6

-- Commande concat
{-Cas de base-}
concat' [] = []
{-Quand le premier tableau est vide, on lance l'appelle récursif avec le second tableau-}
concat' ([]:xs) = concat'(xs)
{-On s'occupe du premier tableau , on prend la tete et on lance l'appelle recursif-}
concat' ((x:xs):ys) = x:concat'(xs:ys)


-- Commande !!
index' n [] = "error index"
index' 0 (x:xs) = x
{-Attention, les parenthèses du n-1 ne sont pas optionnels !-}
index' n (x:xs) = index' (n-1) xs

-- Commande ++
concatv2 [] [] = []
concatv2 [] (x:xs) = x:concatv2 [] xs
concatv2 (x:xs) (y:ys) = x:concatv2 xs (y:ys)


-- Commande map

map' _ [] = []
map' f (x:xs) = f x:map' f xs
{-f est une fonction que l'on applique à notre tête de liste-}

--Q7

{- applique la fonction (!!) l a x on peut ensuite avoir l'élément situé a l'index i avec x i-}

--Q8
{-
longueur [] = 0
longueur (x:xs) = sum [1 | x<-xs]
-}
longueur [] = 0
longueur l = sum(map(const 1) l)


--Q9

f :: a -> a
f a = a
fonct :: (a -> a) -> a -> Int -> [a]
fonct f x n = [y | y <- take n(iterate(f) x)]

fonct' f x 1 = [x]
fonct' f x n = [x] ++ map f (fonct' f x (n-1))

--Q10

somme0aN 0 = [0]
somme0aN n = fonct' (+1) 0 (n+1) 

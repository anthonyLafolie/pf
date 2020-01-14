--Q3

sommeDeXaY :: Int -> Int -> Int
sommeDeXaY x y = sum[x..y]

--Q4
{- Le type ici n'est pas celui attendu -}

somme :: [Int] -> Int
somme [] = 0
somme (x:xs) = x + (somme xs)
--Q5

last' :: [a] -> a
last' xs = head (reverse xs)

init' :: [a] -> [a]
init'[_] = []
init' xs = take(length(xs)-1) xs

init'' :: [a] -> [a]
init'' xs = reverse(drop 1(reverse xs))

--Q6

-- Commande concat

concat' :: [[a]] -> [a]
{-Cas de base-}
concat' [] = []
{-Quand le premier tableau est vide, on lance l'appelle récursif avec le second tableau-}
concat' ([]:xs) = concat'(xs)
{-On s'occupe du premier tableau , on prend la tete et on lance l'appelle recursif-}
concat' ((x:xs):ys) = x:concat'(xs:ys)


-- Commande !!
index' :: Int -> [a] -> a
index' _ [] = error "Erreur"
index' 0 (x:_) = x
{-Attention, les parenthèses du n-1 ne sont pas optionnels !-}
index' n (_:xs) = index' (n-1) xs

-- Commande ++
plusplus :: [a] -> [a] -> [a]
plusplus [] [] = []
plusplus [] (x:xs) = x:plusplus [] xs
plusplus (x:xs) ys = x:plusplus xs ys


-- Commande map
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f1 (x:xs) = f1 x:map' f1 xs
{-f est une fonction que l'on applique à notre tête de liste-}

--Q7

{- applique la fonction (!!) l a x on peut ensuite avoir l'élément situé a l'index i avec x i-}

--Q8
{-
longueur [] = 0
longueur (x:xs) = sum [1 | x<-xs]
-}
longueur :: [a] -> Int
longueur [] = 0
longueur l = somme(map(const 1) l)


--Q9

f :: a -> a
f a = a
fonct :: (a -> a) -> a -> Int -> [a]
fonct f1 x n = [y | y <- take n(iterate(f1) x)]

fonct' :: (a -> a) -> a -> Int -> [a]
{- On pose notre cas de base et sinon on concatene la liste [x] avec la liste n*f[x]-}
fonct' _ x 1 = [x]
fonct' f1 x n = [x] ++ map f1 (fonct' f1 x (n-1))

--Q10
somme0aN :: Int -> [Int]
somme0aN 0 = [0]
somme0aN n = fonct' (+1) 0 (n+1) 

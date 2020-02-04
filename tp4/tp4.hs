import Test.QuickCheck
-- q1
data Arbre coul val = Feuille | Noeud coul val (Arbre coul val) (Arbre coul val) deriving Show 



-- q2



-- q3
hauteur :: Arbre coul val -> Int
hauteur (Noeud _ _ gauche droite) = 1 + (hauteur gauche) `max` (hauteur droite)
hauteur Feuille = 0 

taille :: Arbre coul val -> Int
taille (Noeud _ _ gauche droite) = 1 + hauteur gauche + hauteur droite
taille Feuille = 0 


dimension :: ( Int -> Int -> Int ) -> Arbre coul val ->  Int
dimension f (Noeud _ _ gauche droite)  = 1 + (dimension f gauche) `f` (dimension f droite)
dimension _ Feuille = 0 


-- q5 

peigneGauche :: [(coul,val)] -> Arbre coul val
peigneGauche [] = Feuille
peigneGauche ((coul,val):ns) = (Noeud coul val (peigneGauche ns) Feuille)

--prop_hauteurPeigne xs = length xs == dimension (max) (peigneGauche xs)

prop_hauteurPeigne [] = length [] == hauteur (peigneGauche [])

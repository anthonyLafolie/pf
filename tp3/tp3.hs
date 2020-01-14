import Graphics.Gloss

type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]

motSuivant :: Regles -> Mot -> Mot
motSuivant r [x] = r x
motSuivant r (x:xs) = (r x) ++ motSuivant r xs

motSuivant' :: Regles -> Mot -> Mot
motSuivant' r l = concatMap r l

motSuivant'' :: Regles -> Mot -> Mot
motSuivant'' r l = concat [ r i | i <- l] 

vanKoch :: Symbole -> Mot 
vanKoch '+' = "+"
vanKoch '-' = "-"
vanKoch 'F' = "F-F++F-F"

lsysteme :: Axiome -> Regles -> LSysteme
lsysteme a r = iterate (motSuivant r) a



type EtatTortue = (Point, Float)
type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue

etatInitial :: Config -> EtatTortue
etatInitial (etat, _, _, _, _) =  etat

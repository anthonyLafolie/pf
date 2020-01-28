import Graphics.Gloss

type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]

type EtatTortue = (Point, Float)
type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue

type EtatDessin = ([EtatTortue], [Path])

motSuivant :: Regles -> Mot -> Mot
motSuivant r [] = []
motSuivant r (x:xs) = (r x) ++ motSuivant r xs

motSuivant' :: Regles -> Mot -> Mot
motSuivant' r l = concatMap r l

motSuivant'' :: Regles -> Mot -> Mot
motSuivant'' r l = concat [ r i | i <- l]

vanKoch :: Symbole -> Mot
vanKoch '+' = "+"
vanKoch '-' = "-"
vanKoch 'F' = "F-F++F-F"
vanKoch _ = error "mauvais symbole"

lsysteme :: Axiome -> Regles -> LSysteme
lsysteme a r = iterate (motSuivant' r) a


-- Q4
etatInitial :: Config -> EtatTortue
etatInitial (etat, _, _, _, _) =  etat

longueurPas :: Config -> Float
longueurPas (_, longueur, _, _, _) =  longueur

facteurEchelle :: Config -> Float
facteurEchelle (_, _, facteur, _, _) =  facteur

angle :: Config -> Float
angle (_, _, _, angle, _) =  angle

symbolesTortue :: Config -> [Symbole]
symbolesTortue (_, _, _, _, symboles) =  symboles

-- Q5
avance :: Config -> EtatTortue -> EtatTortue
avance conf ((x,y), cap) = ((x + longueurPas conf *cos(cap),y + longueurPas conf *sin(cap)), cap)

-- Q6
tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche conf (point, cap) = (point, cap + angle conf)

tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite conf (point, cap) = (point, cap - angle conf)

-- Q7
myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect [] _ = []
myIntersect (x:xs) ys
  | x `elem` ys = x : myIntersect xs ys
  | otherwise = myIntersect xs ys

filtreSymbolesTortue :: Config -> Mot -> Mot
filtreSymbolesTortue conf mot = myIntersect mot (symbolesTortue conf)

-- Q8
interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymbole c (et:ets,p:ps) s
                   | s == 'F' = let nE = (avance c et) in (nE:et:ets, [fst(nE)] ++ p)
                   | s == '+' = (tourneAGauche c et:et:ets ,p)
                   | s == '-' = (tourneADroite c et:et:ets,p)

-- Q9
{-  -}

-- Q10
interpreteMot :: Config -> Mot -> Picture
interpreteMot conf mot = line(head(snd( foldl (interpreteSymbole conf) initialEtatDessin motFiltre)))
  where initialEtatDessin = ([etatInitial conf], [[fst(etatInitial conf)]])
        motFiltre = filtreSymbolesTortue conf mot

interpreteMot2 :: Config -> Mot -> [Path]
interpreteMot2 conf mot = snd( foldl (interpreteSymbole conf) initialEtatDessin motFiltre)
  where initialEtatDessin = ([etatInitial conf], [[fst(etatInitial conf)]])
        motFiltre = filtreSymbolesTortue conf mot


dessin = interpreteMot (((-150,0),0),100,1,pi/3,"F+-") "F+F--F+F"

main = animate (InWindow "L-système" (1000, 1000) (0, 0)) white hilbertAnime


lsystemeAnime :: LSysteme -> Config -> Float -> Picture
lsystemeAnime lsys (e,l,fE,a,s) x = interpreteMot conf (lsys !! enieme)
  where enieme = round x `mod` 8
        conf = (e,l * (fE ^ enieme),fE,a,s)



vonKoch1 :: LSysteme
vonKoch1 = lsysteme "F" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

vonKoch2 :: LSysteme
vonKoch2 = lsysteme "F++F++F++" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

hilbert :: LSysteme
hilbert = lsysteme "X" regles
    where regles 'X' = "+YF-XFX-FY+"
          regles 'Y' = "-XF+YFY+FX-"
          regles  s  = [s]

dragon :: LSysteme
dragon = lsysteme "FX" regles
    where regles 'X' = "X+YF+"
          regles 'Y' = "-FX-Y"
          regles  s  = [s]

vonKoch1Anime :: Float -> Picture
vonKoch1Anime = lsystemeAnime vonKoch1 (((-400, 0), 0), 800, 1/3, pi/3, "F+-")

vonKoch2Anime :: Float -> Picture
vonKoch2Anime = lsystemeAnime vonKoch2 (((-400, -250), 0), 800, 1/3, pi/3, "F+-")

hilbertAnime :: Float -> Picture
hilbertAnime = lsystemeAnime hilbert (((-400, -400), 0), 800, 1/2, pi/2, "F+-")

dragonAnime :: Float -> Picture
dragonAnime = lsystemeAnime dragon (((0, 0), 0), 50, 1, pi/2, "F+-")

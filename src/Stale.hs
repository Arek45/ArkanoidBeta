module Stale where
import Graphics.Gloss.Interface.Pure.Game

--ogolne
szerokoscOkna :: Int
szerokoscOkna = 1600

wysokoscOkna :: Int 
wysokoscOkna = 900

trybWyswietlania :: Display
trybWyswietlania = InWindow "Arkanoid" (szerokoscOkna, wysokoscOkna) (0, 0)

szerokoscOknaFloat :: Float
szerokoscOknaFloat = fromIntegral szerokoscOkna

wysokoscOknaFloat :: Float 
wysokoscOknaFloat = fromIntegral wysokoscOkna

szerokoscObszaruGry :: Float
szerokoscObszaruGry = szerokoscOknaFloat / 1600 * 400  

wysokoscObszaruGry :: Float
wysokoscObszaruGry = wysokoscOknaFloat / 900  * 600 

kolorTla :: Color
kolorTla = black

wspolczynnikPredkosci :: Float
wspolczynnikPredkosci = 65

fps :: Int --liczba krokow symulacji wykonanych w ciagu sekundy
fps = 120


--deska

wysokoscDeski :: Float
wysokoscDeski = 10

dlugoscDeski :: Float
dlugoscDeski = 100

wyjsciowaPozycjaDeski :: (Float, Float)
wyjsciowaPozycjaDeski = (0, -250)

predkoscDeski :: Float
predkoscDeski = 400

zakresKataUderzeniaDeski :: (Float, Float)
zakresKataUderzeniaDeski = (-4.3 * wspolczynnikPredkosci, 4.3 * wspolczynnikPredkosci)


--pilka

promienPilki :: Float
promienPilki = 5

wyjsciowaPozycjaPilki :: Point
wyjsciowaPozycjaPilki = (0,-100)


--klocek

wysokoscKlocka :: Float
wysokoscKlocka = 15

szerokoscKlocka ::Float
szerokoscKlocka = 50

szerokoscSciany :: Float
szerokoscSciany = 5





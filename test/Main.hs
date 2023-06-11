module Main where

import Test.QuickCheck
import Stale
import DzialanieGry
import Graphics.Gloss.Interface.Pure.Game
import Data.Fixed
import System.Random
import TypyDanych

instance Arbitrary StatusGry where
  arbitrary = elements [Nieukonczona, Wygrana, Przegrana]

instance Arbitrary StopienTrudnosci where
  arbitrary = elements [Latwy, Sredni, Trudny]

instance Arbitrary Widok where
  arbitrary = elements [Menu, WprowadzanieNicku, WyborPoziomu, WyborStopniaTrudnosci, Instrukcja, Poziom, Pauza, KomunikatOWygranej, KomunikatOPrzegranej, Wyjscie]

instance Arbitrary WcisnietyKlawisz where
  arbitrary = elements [WcisnietyKlawiszLewy, WcisnietyKlawiszPrawy, NieWcisnietyZadenKlawisz]

instance Arbitrary StanGry where
  arbitrary = do
    nickGracza <- arbitrary
    osobistyRekordGracza <- arbitrary
    rekordGlobalny <- arbitrary
    nazwaPlikuZWynikami <- arbitrary
    numerPoziomu <- arbitrary
    stopienTrudnosci <- arbitrary
    statusGry <- arbitrary
    czyGraJestZapisana <- arbitrary
    aktualnyWidok <- arbitrary
    --aktualnaPozycjaPilki
    x <- choose (-szerokoscObszaruGry/2, szerokoscObszaruGry/2)
    y <- choose (-wysokoscObszaruGry/2, wysokoscObszaruGry/2)
    --aktualnyWektorPredkosciPilki
    let v = -(snd $ wyjsciowaPredkoscPilki stopienTrudnosci)
    xDelta <- choose (-v, v)
    let yDelta = sqrt (v * v - xDelta * xDelta)
    --aktualnaPozycjaDeski
    xD <- choose (-szerokoscObszaruGry/2+dlugoscDeski/2, szerokoscObszaruGry/2-dlugoscDeski/2)
    aktualnaPlanszaKlockow <- arbitrary
    wcisnieteKlawisze <- arbitrary
    StanGry nickGracza osobistyRekordGracza rekordGlobalny nazwaPlikuZWynikami numerPoziomu stopienTrudnosci 0 statusGry czyGraJestZapisana aktualnyWidok (x,y) (xDelta,yDelta) (xD, -250) aktualnaPlanszaKlockow wcisnieteKlawisze <$> arbitrary

instance Arbitrary Klocek where
  arbitrary = do
    pozycja <- arbitrary
    rozmiar <- arbitrary
    Klocek pozycja rozmiar <$> arbitrary

instance Arbitrary PlanszaKlockow where
  arbitrary = do
    klocki <- arbitrary
    PlanszaKlockow klocki <$> arbitrary

instance Arbitrary Uderzenie where
  arbitrary = elements [UderzenieGora, UderzenieDol, UderzenieLewa, UderzeniePrawa, UderzenieLewaGora, UderzeniePrawaGora, UderzenieLewyDol, UderzeniePrawyDol, UderzeniePlatformy, BrakUderzenia]

instance Arbitrary RzadKlockowPoUderzeniu where
  arbitrary = do
    rzad <- arbitrary
    RzadKlockowPoUderzeniu rzad <$> arbitrary

instance Arbitrary DeskaUderzenie where
  arbitrary = do
    czyPilkaUderzaWDeske <- arbitrary
    DeskaUderzenie czyPilkaUderzaWDeske <$> arbitrary

--Czy pilka nie wypada poza obszar gry?
test_CzyPilkaNieWypadaPozaObszarGry :: StanGry -> Bool
test_CzyPilkaNieWypadaPozaObszarGry s =
  fst (przemieszczeniePilki (aktualnaPozycjaPilki s) (aktualnyWektorPredkosciPilki s)) > (-szerokoscObszaruGry/2) &&
  fst (przemieszczeniePilki (aktualnaPozycjaPilki s) (aktualnyWektorPredkosciPilki s)) < szerokoscObszaruGry/2 &&
  snd (przemieszczeniePilki (aktualnaPozycjaPilki s) (aktualnyWektorPredkosciPilki s)) > (-300) &&
  snd (przemieszczeniePilki (aktualnaPozycjaPilki s) (aktualnyWektorPredkosciPilki s)) < 300

test_CzyPilkaZachowujeStalaDlugoscWektoraPredkosci :: Point -> StanGry -> Uderzenie -> Bool
test_CzyPilkaZachowujeStalaDlugoscWektoraPredkosci p s u =
  sqrt (nowaXDelta1 * nowaXDelta1 + nowaYDelta1 * nowaYDelta1) >= v-blad &&
  sqrt (nowaXDelta1 * nowaXDelta1 + nowaYDelta1 * nowaYDelta1) <= v+blad &&
  sqrt (nowaXDelta2 * nowaXDelta2 + nowaYDelta2 * nowaYDelta2) >= v-blad &&
  sqrt (nowaXDelta2 * nowaXDelta2 + nowaYDelta2 * nowaYDelta2) <= v+blad
  where
    v = - (snd $ wyjsciowaPredkoscPilki (stopienTrudnosci s))
    blad = abs v/100
    nowaXDelta1 = fst $ zmianaKierunkuPilki (aktualnaPozycjaPilki s) (aktualnyWektorPredkosciPilki s) u
    nowaYDelta1 = snd $ zmianaKierunkuPilki (aktualnaPozycjaPilki s) (aktualnyWektorPredkosciPilki s) u
    nowaXDelta2 = if czyPilkaUderzaWDeske (odbicieOdDeski p s) then fst $ kierunekPoOdbiciu $ odbicieOdDeski p s else nowaXDelta1
    nowaYDelta2 = if czyPilkaUderzaWDeske (odbicieOdDeski p s) then snd $ kierunekPoOdbiciu $ odbicieOdDeski p s else nowaYDelta1

test_CzyDeskaWCalosciMiesciSieWObszarzeGry :: StanGry -> Bool
test_CzyDeskaWCalosciMiesciSieWObszarzeGry s =
  lewyKraniecDeski >= lewaKrawedzObszaruGry &&
  prawyKraniecDeski <= prawaKrawedzObszaruGry
  where
    lewyKraniecDeski = fst (aktualnaPozycjaDeski s) + dlugoscDeski/2
    prawyKraniecDeski = fst (aktualnaPozycjaDeski s) - dlugoscDeski/2
    lewaKrawedzObszaruGry = -szerokoscObszaruGry  / 2
    prawaKrawedzObszaruGry = szerokoscObszaruGry / 2

test_CzyJesliPilkaSiegnieDnaToPrzegrana :: Point -> StanGry -> Bool
test_CzyJesliPilkaSiegnieDnaToPrzegrana (x,y) s =
  (y - promienPilki) > (-wysokoscObszaruGry/2) || czyPilkaUpadnie (x,y) s

test_CzyLiczbaKlockowNaPlanszyJestNieujemna :: PlanszaKlockow -> Bool
test_CzyLiczbaKlockowNaPlanszyJestNieujemna p =
  ileZostaloKlockowNaPlanszy p >= 0

test_CzyPoUderzeniuRzadKlockowZmniejszaSieoMax1Element :: Point -> RzadKlockow -> Bool
test_CzyPoUderzeniuRzadKlockowZmniejszaSieoMax1Element p r =
  length rzadPo == length rzadPrzed ||
  length rzadPo == length rzadPrzed + 1 ||
  length rzadPo == length rzadPrzed - 1
  where
    rzadPo = filter (/= BrakKlocka) $ rzad (rzadKlockowPoUderzeniu p r)
    rzadPrzed = filter (/= BrakKlocka) r

test_CzyPoUderzeniuPlanszaKlockowZmniejszaSieoMax1Element :: Point -> [RzadKlockow] -> Bool
test_CzyPoUderzeniuPlanszaKlockowZmniejszaSieoMax1Element p rs =
  length planszaPo == length planszaPrzed ||
  length planszaPo == length planszaPrzed + 1 ||
  length planszaPo == length planszaPrzed - 1
  where
    planszaPo = filter (/= BrakKlocka) $ concat $ klocki (planszaKlockowPoUderzeniu p rs)
    planszaPrzed = filter (/= BrakKlocka) $ concat rs

test_CzyDeskaPoruszaSiePrawidlowo :: StanGry -> Bool
test_CzyDeskaPoruszaSiePrawidlowo s =
  fst pozycjaDeskiPoMozliwymRuchuWLewo <= fst pozycjaDeskiPrzedRuchem &&
  fst pozycjaDeskiPoNiemozliwymRuchuWLewo == fst pozycjaDeskiPrzedRuchem &&
  fst pozycjaDeskiPoMozliwymRuchuWPrawo >= fst pozycjaDeskiPrzedRuchem &&
  fst pozycjaDeskiPoNiemozliwymRuchuWPrawo == fst pozycjaDeskiPrzedRuchem &&
  snd pozycjaDeskiPoMozliwymRuchuWLewo == snd pozycjaDeskiPrzedRuchem &&
  snd pozycjaDeskiPoNiemozliwymRuchuWLewo == snd pozycjaDeskiPrzedRuchem &&
  snd pozycjaDeskiPoMozliwymRuchuWPrawo == snd pozycjaDeskiPrzedRuchem &&
  snd pozycjaDeskiPoNiemozliwymRuchuWPrawo == snd pozycjaDeskiPrzedRuchem  
  where
    lewaKrawedzObszaruGry = -szerokoscObszaruGry  / 2
    prawaKrawedzObszaruGry = szerokoscObszaruGry / 2
    pozycjaDeskiPrzedRuchem = aktualnaPozycjaDeski s
    cokolwiek = 1
    pozycjaDeskiPoMozliwymRuchuWLewo =
      if fst pozycjaDeskiPrzedRuchem > lewaKrawedzObszaruGry + dlugoscDeski/2 then aktualnaPozycjaDeski (ruchDeskiWLewo s) else  (fst pozycjaDeskiPrzedRuchem - cokolwiek, snd pozycjaDeskiPrzedRuchem)
    pozycjaDeskiPoNiemozliwymRuchuWLewo =
      if fst pozycjaDeskiPrzedRuchem == lewaKrawedzObszaruGry + dlugoscDeski/2 then aktualnaPozycjaDeski (ruchDeskiWLewo s) else pozycjaDeskiPrzedRuchem
    pozycjaDeskiPoMozliwymRuchuWPrawo =
      if fst pozycjaDeskiPrzedRuchem < prawaKrawedzObszaruGry - dlugoscDeski/2 then aktualnaPozycjaDeski (ruchDeskiWPrawo s) else (fst pozycjaDeskiPrzedRuchem + cokolwiek, snd pozycjaDeskiPrzedRuchem)
    pozycjaDeskiPoNiemozliwymRuchuWPrawo =
      if fst pozycjaDeskiPrzedRuchem == prawaKrawedzObszaruGry - dlugoscDeski/2 then aktualnaPozycjaDeski (ruchDeskiWPrawo s) else pozycjaDeskiPrzedRuchem


-- Uruchamianie testu
main :: IO ()
main = do
  quickCheck test_CzyPilkaNieWypadaPozaObszarGry
  quickCheck test_CzyPilkaZachowujeStalaDlugoscWektoraPredkosci
  quickCheck test_CzyDeskaWCalosciMiesciSieWObszarzeGry
  quickCheck test_CzyJesliPilkaSiegnieDnaToPrzegrana
  quickCheck test_CzyLiczbaKlockowNaPlanszyJestNieujemna
  quickCheck test_CzyPoUderzeniuRzadKlockowZmniejszaSieoMax1Element
  quickCheck test_CzyPoUderzeniuPlanszaKlockowZmniejszaSieoMax1Element
  quickCheck test_CzyDeskaPoruszaSiePrawidlowo
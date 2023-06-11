module TypyDanych where

import Data.Fixed
import Data.Time.Clock
import Graphics.Gloss.Interface.Pure.Game

data StatusGry =
    Nieukonczona | Wygrana | Przegrana  deriving (Eq, Show)

data StopienTrudnosci = Latwy | Sredni | Trudny | BrakStopniaTrudnosci deriving (Eq, Show)

data Widok =
    Menu | WprowadzanieNicku | WyborPoziomu | WyborStopniaTrudnosci | Instrukcja | Poziom | Pauza | KomunikatOWygranej | KomunikatOPrzegranej | Wyjscie deriving (Eq, Show)

data WcisnietyKlawisz = WcisnietyKlawiszLewy | WcisnietyKlawiszPrawy | NieWcisnietyZadenKlawisz deriving (Eq, Show)
type WcisnieteKlawisze = [WcisnietyKlawisz] 

data StanGry = StanGry {
    nickGracza :: String, 
    osobistyRekordGracza  :: Pico, --najlepszy gracza w dla danego poziomu i stopnia trudnosci
    rekordGlobalny :: Pico, --najlepszy rekord w dla danego poziomu i stopnia trudnosci
    nazwaPlikuZWynikami :: String, --nazwa pliku z wynikami dla danego poziomu i stopnia trundosci
    numerPoziomu :: Int,
    stopienTrudnosci :: StopienTrudnosci,
    czasGry :: NominalDiffTime, 
    statusGry :: StatusGry, 
    czyGraJestZapisana :: Bool, 
    aktualnyWidok :: Widok, 
    aktualnaPozycjaPilki :: Point, 
    aktualnyWektorPredkosciPilki :: Vector, 
    aktualnaPozycjaDeski :: Point, 
    aktualnaPlanszaKlockow :: PlanszaKlockow, 
    wcisnieteKlawisze :: WcisnieteKlawisze,
    liczbaPozostalychKlockowNaPlanszy :: Int
}  deriving Show

data Klocek = Klocek {
    pozycja :: Point,
    rozmiar :: Point,
    liczbaPozostalychUderzen :: Int
} | BrakKlocka deriving (Eq, Show)

type RzadKlockow = [Klocek] 

data PlanszaKlockow = PlanszaKlockow {
    klocki :: [RzadKlockow],
    ostatnieUderzenie :: Uderzenie
} deriving (Eq, Show)
--nazwy wiaza sie z kierunkiem w ktorym zmierzala pilka przed uderzenie
data Uderzenie = 
    UderzenieGora | UderzenieDol | UderzenieLewa | UderzeniePrawa | UderzenieLewaGora | UderzeniePrawaGora | UderzenieLewyDol | UderzeniePrawyDol | UderzeniePlatformy | BrakUderzenia deriving (Eq,Show)


data RzadKlockowPoUderzeniu = RzadKlockowPoUderzeniu{
    rzad :: RzadKlockow,
    uderzenie :: Uderzenie
} deriving (Eq, Show)

data DeskaUderzenie = DeskaUderzenie {
    czyPilkaUderzaWDeske :: Bool,
    kierunekPoOdbiciu :: Point  -- nowy kierunek pilki po odbiciu od platformy
} deriving (Eq, Show)


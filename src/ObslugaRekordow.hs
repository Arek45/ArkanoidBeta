{-# LANGUAGE RecordWildCards #-}
module ObslugaRekordow where

import System.Directory ( createDirectoryIfMissing, doesFileExist )
import Data.List
import Data.Fixed
import Data.Maybe (isNothing)
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad (guard)

import Stale
import TypyDanych

--tworzenie pliku i zapisywanie nowych wynikow
utworzPlikDoZapisuWynikow :: String -> IO ()
utworzPlikDoZapisuWynikow sciezka = do
    createDirectoryIfMissing False "ZapisWynikow"
    appendFile sciezka ""
    return ()

zapiszWynik :: StanGry -> String -> IO ()
zapiszWynik stan@StanGry{..} sciezka =
    if not czyGraJestZapisana then do
        createDirectoryIfMissing False "ZapisWynikow"
        appendFile sciezka ( nickGracza ++ "   " ++ (formatujCzasDoWyswietlenia (show czasGry)) ++ "\n")
    else return () 

formatujWynikiZPliku :: String -> String
formatujWynikiZPliku [] = ""
formatujWynikiZPliku ('s':'e':'k':'.':xs) = 's':'e':'k':'.':formatujWynikiZPliku xs
formatujWynikiZPliku ('.':x:xs) = '.':x:formatujWynikiZPliku (dropWhile (/=' ') xs)
formatujWynikiZPliku (x:xs) = x:formatujWynikiZPliku xs



--wyswietlanie wynikow, czasu gry, pb gracza oraz globalnego rekordu w aplikacji

wyswietlWRzedach :: Float -> Float -> [Picture] -> [Picture]
wyswietlWRzedach _ _ [] = []
wyswietlWRzedach wysokoscPierwszegoWiersza odstep  (x:xs) = Translate 0 wysokoscPierwszegoWiersza x : wyswietlWRzedach (wysokoscPierwszegoWiersza - odstep) odstep xs

formatujCzasDoWyswietlenia :: String -> String
formatujCzasDoWyswietlenia [] = []
formatujCzasDoWyswietlenia (x:xs) = 
    case x of
        '.' -> x : head xs : " sek."
        _   -> x : formatujCzasDoWyswietlenia xs

wszystkieRekordy :: String -> [Pico]
wszystkieRekordy [] = []
wszystkieRekordy zawartoscPliku = (read czasBezJednostki :: Pico): wszystkieRekordy pozostaleLinijki
    where
        aktualnaLinijka = takeWhile (/='\n') zawartoscPliku
        pozostaleLinijki = drop 1 (dropWhile (/='\n') zawartoscPliku)
        czasBezJednostki = takeWhile (/='s') (drop 1 (dropWhile (/=' ') aktualnaLinijka))
        
najlepszyRekord :: String -> IO Pico
najlepszyRekord sciezka = do 
    zawartoscPliku <- readFile sciezka
    let listaRekordow = wszystkieRekordy zawartoscPliku
    if not $ null listaRekordow then return (minimum listaRekordow)
    else return 0

rekordyGracza :: String -> String -> [Pico]
rekordyGracza _ [] = []
rekordyGracza nick zawartoscPliku 
    | nick == aktualnyNick = (read czasBezJednostki :: Pico): rekordyGracza nick pozostaleLinijki
    | otherwise = rekordyGracza nick pozostaleLinijki
        where
            aktualnaLinijka = takeWhile (/='\n') zawartoscPliku
            pozostaleLinijki = drop 1 (dropWhile (/='\n') zawartoscPliku)
            aktualnyNick = takeWhile (/=' ') zawartoscPliku
            czasBezJednostki = takeWhile (/='s') (drop 1 (dropWhile (/=' ') aktualnaLinijka))

najlepszyRekordGracza :: String -> String -> IO Pico
najlepszyRekordGracza im sciezka = do 
    zawartoscPliku <- readFile sciezka
    let listaRekordowGracza = rekordyGracza im zawartoscPliku
    if not $ null listaRekordowGracza then return (minimum listaRekordowGracza)
    else return 0

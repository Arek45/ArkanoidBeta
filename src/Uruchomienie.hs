{-# LANGUAGE RecordWildCards #-}

module Uruchomienie where

import System.Exit
import Data.Time.Clock
import Data.List ( delete )
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy

import Stale
import TypyDanych
import DzialanieGry
import GenerowanieObiektow
import ObslugaRekordow

--zamiana napisu ze stopniem trudnosci na typ StopienTrudnosci
stopienTrudnosciPole :: String -> StopienTrudnosci
stopienTrudnosciPole stopienNapis = case stopienNapis of
    "latwy" -> Latwy
    "sredni" -> Sredni
    "trudny" -> Trudny
    _ -> BrakStopniaTrudnosci

stanWejsciaDoMenuPierwszyRaz :: StanGry
stanWejsciaDoMenuPierwszyRaz = StanGry "" 0 0 "" 0 BrakStopniaTrudnosci (secondsToNominalDiffTime 0) Nieukonczona False Menu wyjsciowaPozycjaPilki (0,0) wyjsciowaPozycjaDeski (PlanszaKlockow [] BrakUderzenia) [NieWcisnietyZadenKlawisz] (-1)

stanWejsciaDoMenuKolejnyRaz :: StanGry -> StanGry
stanWejsciaDoMenuKolejnyRaz stan = StanGry (nickGracza stan) 0 0 "" 0 BrakStopniaTrudnosci (secondsToNominalDiffTime 0) Nieukonczona False Menu wyjsciowaPozycjaPilki (0,0) wyjsciowaPozycjaDeski (PlanszaKlockow [] BrakUderzenia) [NieWcisnietyZadenKlawisz] (-1)


--stan nowej gry po wyjsciu z ekranow menu
stanNowejGry :: StanGry ->  IO StanGry
stanNowejGry stan@StanGry{..}  = case nazwaPlikuZWynikami of
    "" -> return stan
    _ -> do
            let ileUciac = length "ZapisWynikow/wyniki"
                nrPoziomu = read $ take 1 $ drop ileUciac nazwaPlikuZWynikami
                stTrudnosci  = stopienTrudnosciPole $ takeWhile (/= '.') $ drop (ileUciac+1) nazwaPlikuZWynikami
            () <- utworzPlikDoZapisuWynikow nazwaPlikuZWynikami
            pb <- najlepszyRekordGracza nickGracza nazwaPlikuZWynikami
            rekord <- najlepszyRekord nazwaPlikuZWynikami 
            return $ stan{osobistyRekordGracza = pb, rekordGlobalny = rekord, numerPoziomu = nrPoziomu, stopienTrudnosci = stTrudnosci, czasGry = secondsToNominalDiffTime 0, aktualnyWidok = Poziom, aktualnaPlanszaKlockow = generujPoziom nrPoziomu, liczbaPozostalychKlockowNaPlanszy = ileZostaloKlockowNaPlanszy $ generujPoziom nrPoziomu, aktualnyWektorPredkosciPilki = wyjsciowaPredkoscPilki stTrudnosci, wcisnieteKlawisze = [NieWcisnietyZadenKlawisz]}

stanZrestartowanejGry :: StanGry ->  IO StanGry
stanZrestartowanejGry stan@StanGry{..}   = case nazwaPlikuZWynikami of
    "" -> return stan
    _ -> do
            let ileUciac = length "ZapisWynikow/wyniki"
                nrPoziomu = read $ take 1 $ drop ileUciac nazwaPlikuZWynikami
                stTrudnosci  = stopienTrudnosciPole $ takeWhile (/= '.') $ drop (ileUciac+1) nazwaPlikuZWynikami
            return $ stan{czasGry = secondsToNominalDiffTime 0, statusGry = Nieukonczona, czyGraJestZapisana = False, aktualnyWidok = Poziom, aktualnaPozycjaPilki = wyjsciowaPozycjaPilki, aktualnyWektorPredkosciPilki = wyjsciowaPredkoscPilki stTrudnosci, aktualnaPozycjaDeski = wyjsciowaPozycjaDeski, aktualnaPlanszaKlockow = generujPoziom nrPoziomu, liczbaPozostalychKlockowNaPlanszy = ileZostaloKlockowNaPlanszy $ generujPoziom nrPoziomu, wcisnieteKlawisze = [NieWcisnietyZadenKlawisz]}

--zmiana stanu gry po kliknieciu
iteracja :: Float -> StanGry -> IO StanGry
iteracja s stan@StanGry{..}
    | aktualnyWidok == Wyjscie = do --wyjscie z gry
        exitSuccess
        return stan
    | aktualnyWidok /= Poziom = return stan --jesli nie gramy, to nic nie zmienia sie na ekranie (tryb Poziom jest w momencie grania, Wygranej lub Przegranej)
    | statusGry == Wygrana = do
        zapiszWynik stan nazwaPlikuZWynikami
        let nowyPB = if osobistyRekordGracza == 0 then nominalDiffTimeToSeconds czasGry else min osobistyRekordGracza (nominalDiffTimeToSeconds czasGry)
        let nowyRekord = if rekordGlobalny == 0 then nominalDiffTimeToSeconds czasGry else min rekordGlobalny (nominalDiffTimeToSeconds czasGry)
        return $ StanGry nickGracza nowyPB nowyRekord nazwaPlikuZWynikami numerPoziomu stopienTrudnosci czasGry Wygrana True KomunikatOWygranej aktualnaPozycjaPilki (0,0) aktualnaPozycjaDeski aktualnaPlanszaKlockow [NieWcisnietyZadenKlawisz] 0
    | statusGry == Przegrana = return $ StanGry nickGracza osobistyRekordGracza rekordGlobalny nazwaPlikuZWynikami numerPoziomu stopienTrudnosci czasGry Przegrana czyGraJestZapisana KomunikatOPrzegranej aktualnaPozycjaPilki (0,0) aktualnaPozycjaDeski aktualnaPlanszaKlockow [NieWcisnietyZadenKlawisz] 0
    --przypadek otherwise to sytuacja w trakcie rozgrywki
    | otherwise = return $ StanGry nickGracza osobistyRekordGracza rekordGlobalny nazwaPlikuZWynikami numerPoziomu stopienTrudnosci (czasGry + secondsToNominalDiffTime (realToFrac s)) nowyStatus czyGraJestZapisana aktualnyWidok nowaPozycjaPilki nowyWektorPredkosciPilki nowaPozycjaDeski nowaPlansza wcisnieteKlawisze nowaLiczbaPozostalychKlockow
        where
            nowaPozycjaPilki = przemieszczeniePilki aktualnaPozycjaPilki aktualnyWektorPredkosciPilki
            nowaPlansza = planszaKlockowPoUderzeniu nowaPozycjaPilki (klocki aktualnaPlanszaKlockow) 
            nowaLiczbaPozostalychKlockow = ileZostaloKlockowNaPlanszy nowaPlansza
            nowaPozycjaDeski = ruchDeski stan
            ostatnieuderzenie = ostatnieUderzenie nowaPlansza
            DeskaUderzenie czyuderzylo kierunekpoodbiciu = odbicieOdDeski nowaPozycjaPilki stan
            rodzajUderzenia | czyuderzylo = UderzeniePlatformy
                            | otherwise = ostatnieuderzenie
            nowyWektorPredkosciPilki | czyuderzylo = kierunekpoodbiciu
                                     | otherwise = zmianaKierunkuPilki nowaPozycjaPilki aktualnyWektorPredkosciPilki rodzajUderzenia
            nowyStatus | czyPilkaUpadnie nowaPozycjaPilki stan = Przegrana
                       | nowaLiczbaPozostalychKlockow == 0 = Wygrana
                       | otherwise = Nieukonczona



narysujGre :: Picture -> Picture -> StanGry ->  IO Picture
narysujGre tloGra tloMenu stan@StanGry{..} = do
    --do wyswietlenia czasu gry
    let czasDoWysietleniaNaEkranie = formatujCzasDoWyswietlenia (show (nominalDiffTimeToSeconds czasGry))
        czasObraz = scale 0.2 0.2 $ translate (-szerokoscObszaruGry * 7) 700 $ color white $ text czasDoWysietleniaNaEkranie
    --do wyswietlenia rekordu globalnego w menu gry
    let rekordDoWyswietleniaNaEkranie = if rekordGlobalny == 0 then "Brak rekordu" else "Rekord: " ++ formatujCzasDoWyswietlenia (show rekordGlobalny)
        rekordObraz = scale  0.2 0.2 $ translate (-szerokoscObszaruGry * 7) (-1100) $ color white $ text rekordDoWyswietleniaNaEkranie
    --do wyswietlenia nowego PB gracza w menu wygranej
        aktualnyCzasGry = nominalDiffTimeToSeconds czasGry
        nowyRekordDoWyswietleniaNaEkranie
          | aktualnyCzasGry <= osobistyRekordGracza && aktualnyCzasGry > rekordGlobalny = "Nowy osobisty rekord!                     " 
          | aktualnyCzasGry <= rekordGlobalny && aktualnyCzasGry <= osobistyRekordGracza = "  Nowy rekord!" 
          | otherwise = ""
        nowyRekordObraz = scale 0.2 0.2 $ translate (- szerokoscObszaruGry / 4 - 550) (-250) $ color white $ text nowyRekordDoWyswietleniaNaEkranie
        --tlo gry, menu
        tloGraObraz =  scale 1 1 $ translate 0 0 tloGra
        tloMenuObraz = scale 1 1 $ translate 0 0 tloMenu
    case aktualnyWidok of
        Menu -> return (pictures [tloMenuObraz, menuObraz, menuInfoRozpocznijObraz])
        WprowadzanieNicku -> return (pictures [tloMenuObraz, wprowadzanieNickuInfo1Obraz, wWprowadzenieNickuNickObraz, wprowadzenieNickuInfo2Obraz])
        WyborPoziomu -> return (pictures [tloMenuObraz, wyborPoziomuObraz, wyborPoziomuPierwszegoObraz, wyborPoziomuDrugiegoObraz, wyborPoziomuTrzeciegoObraz])
        WyborStopniaTrudnosci -> return (pictures [tloMenuObraz, wyborStopniaTrudnosciObraz, wyborStopniaTrudnosciLatwyObraz, wyborStopniaTrudnosciSredniObraz, wyborStopniaTrudnosciTrudnyObraz])
        Instrukcja -> return (pictures [tloMenuObraz, instrukcjaInfoInstrukcjaObraz, instrukcjaInfoSterowanieObraz, instrukcjaInfoRozpocznijObraz])
        Poziom -> return (pictures [tloGraObraz, infoStopienTrudnosciObraz, infoPoziomObraz, rekordObraz, pbObraz, czasObraz, nickGraczaPoziomObraz, pilkaObraz, planszaObraz, deskaObraz, scianyKoloroweObraz, infoMenuObraz, infoPauzaObraz, infoWyjscieObraz,  infoRestartObraz])
        Pauza -> return (pictures [tloGraObraz, infoStopienTrudnosciObraz, infoPoziomObraz, pauzaInfoObraz, rekordObraz, pbObraz, czasObraz, nickGraczaPoziomObraz, pauzaObraz, pilkaObraz, planszaObraz, deskaObraz, scianyKoloroweObraz, infoMenuObraz, infoPauzaObraz, infoWyjscieObraz, infoRestartObraz])
        KomunikatOWygranej -> return (pictures [tloGraObraz, infoStopienTrudnosciObraz, infoPoziomObraz, nowyRekordObraz, rekordObraz, pbObraz, czasObraz, nickGraczaPoziomObraz, wygranaObraz, deskaObraz, scianyKoloroweObraz, infoMenuObraz, infoPauzaObraz, infoWyjscieObraz, infoRestartObraz])
        KomunikatOPrzegranej -> return (pictures [tloGraObraz, infoStopienTrudnosciObraz, infoPoziomObraz, rekordObraz, pbObraz, czasObraz, nickGraczaPoziomObraz, przegranaObraz, pilkaObraz, deskaObraz, scianyKoloroweObraz,  infoMenuObraz, infoPauzaObraz, infoWyjscieObraz, infoRestartObraz])
        _ -> return (pictures [pilkaObraz, planszaObraz, deskaObraz, scianyKoloroweObraz, infoMenuObraz, infoPauzaObraz, infoWyjscieObraz, infoRestartObraz])
    where
        --teksty pomocy w menu glownym
        menuObraz = scale 1 1 $ translate (-480) 240 $ color red $ text "GRA ARKANOID"
        menuInfoRozpocznijObraz = scale 0.45 0.45 $ scale 0.70 0.70 $ translate (-szerokoscObszaruGry * 2.5) (-550) $ color white $ text "Wcisnij ENTER, aby rozpoczac"
        --do wsywietlenia elementow w widoku WprowadzenieNicku
        wprowadzanieNickuInfo1Obraz = scale 0.35 0.35 $ translate (-szerokoscObszaruGry * 1) 300 $ color white $ text "Podaj nick"
        wWprowadzenieNickuNickObraz = scale 0.35 0.35 $ translate (-szerokoscObszaruGry * 1) (100) $ color yellow $ text nickGracza
        wprowadzenieNickuInfo2Obraz = scale 0.35 0.35 $ translate (-szerokoscObszaruGry * 2.5) (-550) $ color white $ text "Wcisnij ENTER, aby zatwierdzic"
        --do wsywietlenia elementow w widoku WyborPoziomu
        wyborPoziomuObraz = scale 0.35 0.35 $ translate (-szerokoscObszaruGry * 1.3) 300 $ color white $ text "Wybierz Poziom"
        wyborPoziomuPierwszegoObraz = scale 0.25 0.25 $ translate (-szerokoscObszaruGry * 1.9) 100 $ color white $ text "Poziom 1 - Wcisnij 1"
        wyborPoziomuDrugiegoObraz = scale 0.25 0.25 $ translate (-szerokoscObszaruGry *1.9) (-100) $ color white $ text "Poziom 2 - Wcisnij 2"
        wyborPoziomuTrzeciegoObraz = scale 0.25 0.25 $ translate (-szerokoscObszaruGry * 1.9) (-300) $ color white $ text "Poziom 3 - Wcisnij 3"
        --do wsywietlenia elementow w widoku WyborStopniaTrudnosci        
        wyborStopniaTrudnosciObraz = scale 0.35 0.35 $ translate (-szerokoscObszaruGry * 2) 300 $ color white $ text "Wybierz stopien trudnosci"
        wyborStopniaTrudnosciLatwyObraz = scale 0.25 0.25 $ translate (-szerokoscObszaruGry * 1.7) 100 $ color white $ text "Latwy - Wcisnij 1"
        wyborStopniaTrudnosciSredniObraz = scale 0.25 0.25 $ translate (-szerokoscObszaruGry * 1.7) (-100) $ color white $ text "Sredni - Wcisnij 2"
        wyborStopniaTrudnosciTrudnyObraz = scale 0.25 0.25 $ translate (-szerokoscObszaruGry * 1.7) (-300) $ color white $ text "Trudny - Wcisnij 3"
        --do wyswietlenia elementow w widoku Instrukcja
        instrukcjaInfoInstrukcjaObraz = scale 0.45 0.45 $ scale 0.70 0.70 $ translate (-szerokoscObszaruGry * 3.9) 300 $ color white $ text "Zniszcz wszystkie klocki w jak najkrotszym czasie"
        instrukcjaInfoSterowanieObraz = scale 0.45 0.45 $ scale 0.70 0.70 $ translate (-szerokoscObszaruGry * 3.3) 100 $ color white $ text "Uzywaj <- oraz ->, by sterowac deska"
        instrukcjaInfoRozpocznijObraz = scale 0.45 0.45 $ scale 0.70 0.70 $ translate (-szerokoscObszaruGry * 2.2) (-550) $ color white $ text "Wcisnij ENTER, aby zagrac"
        --wyswietlanie obiektow w widokach Poziom, Pauza, KomunikatOPrzegranej, KomunikatOWygranej
        pilkaObraz = uncurry translate aktualnaPozycjaPilki $ color white (circleSolid promienPilki)
        planszaObraz = stworzPlansze aktualnaPlanszaKlockow
        scianyKoloroweObraz = color red scianyObrazy
        scianyObrazy= pictures [
            translate 0 (wysokoscObszaruGry / 2.0) (rectangleSolid szerokoscObszaruGry szerokoscSciany),
            translate 0 (- wysokoscObszaruGry / 2.0) (rectangleSolid szerokoscObszaruGry szerokoscSciany),
            translate ((- szerokoscObszaruGry) / 2.0) 0 (rectangleSolid szerokoscSciany wysokoscObszaruGry),
            translate (szerokoscObszaruGry / 2.0) 0 (rectangleSolid szerokoscSciany wysokoscObszaruGry)
            ]
        deskaObraz = pictures [
            uncurry translate aktualnaPozycjaDeski $ color green (rectangleSolid dlugoscDeski wysokoscDeski)]
        nickGraczaPoziomObraz = scale 0.25 0.25 $ translate (-szerokoscObszaruGry * 5.6) 800 $ color yellow $ text nickGracza
        pbDowyswietleniaNaEkranie = if osobistyRekordGracza == 0 then "Brak osobistego rekordu" else "Osobisty rekord: " ++ formatujCzasDoWyswietlenia (show osobistyRekordGracza)
        pbObraz = scale  0.2 0.2 $ translate (-szerokoscObszaruGry * 7) (-900) $ color white $ text pbDowyswietleniaNaEkranie
        infoPoziomObraz = scale 0.2 0.2 $ translate (szerokoscObszaruGry * 3.5) (1000) $ color white $ text $ "Poziom: " ++ show numerPoziomu
        infoStopienTrudnosciObraz = scale  0.2 0.2 $ translate (szerokoscObszaruGry * 3.5) (850) $ color white $ text $ "Stopien trudnosci: " ++ show stopienTrudnosci
        infoMenuObraz = scale 0.2 0.2 $ translate (szerokoscObszaruGry * 3.5) (150-900) $ color white $ text "Menu glowne - M"
        infoPauzaObraz = scale  0.2 0.2 $ translate (szerokoscObszaruGry * 3.5) (0-900) $ color white $ text "Pauza - P"
        infoRestartObraz = scale  0.2 0.2 $ translate (szerokoscObszaruGry * 3.5) (-150-900) $ color white $ text "Restart - R"
        infoWyjscieObraz = scale  0.2 0.2 $ translate (szerokoscObszaruGry * 3.5) (-300-900) $ color white $ text "Wyjscie - Esc"
        --tekst PAUZA w menu pauzy
        pauzaObraz = scale 0.35 0.35 $ translate (-szerokoscObszaruGry * 0.5) 0 $ color white $ text "PAUZA"
        pauzaInfoObraz = scale 0.15 0.15 $ translate (-szerokoscObszaruGry * 2.1) (-250) $ color white $ text "Wcisnij P aby kontunyowac"
        --komunikaty o wygranej i przegranej w menu wygranej i przegranej
        wygranaObraz = scale 0.35 0.35 $ translate (-250) 0 $ color white $ text "Wygrana!"
        przegranaObraz = scale 0.35 0.35 $ translate (-300) 0 $ color white $ text "Przegrana"

--obsluga przyciskow
--data Event - Possible input events.
--EventKey Key KeyState Modifiers (Float, Float)
--Keystate moze byc Down albo Up
--Modifiers to rzeczy typu KeyState (shift, ctrl, alt)
--SpecialKey to specjalne klawisze (wszystkie poza literami)
--delete usuwa pierwsze wystapienie pierwszego argumentu z drugiego argumentu (listy)
obslugaWejscia :: Event -> StanGry -> IO StanGry
obslugaWejscia (EventKey (SpecialKey KeyEnter) Down _ _) stan@StanGry{..}
    | aktualnyWidok == Menu = return (stan {aktualnyWidok = WprowadzanieNicku})
    | aktualnyWidok == WprowadzanieNicku && nickGracza /= [] = return (stan {aktualnyWidok = WyborPoziomu})
    | aktualnyWidok == Instrukcja = stanNowejGry stan
obslugaWejscia (EventKey (SpecialKey KeyEsc) Down _ _) stan@StanGry{..}
    = return (stan {aktualnyWidok = Wyjscie})
obslugaWejscia (EventKey (Char '\b') Down _ _ ) stan@StanGry{..}
    | aktualnyWidok == WprowadzanieNicku && nickGracza /= "" = return stan{nickGracza = init nickGracza}    
obslugaWejscia (EventKey (SpecialKey key) keyState _ _) stan@StanGry{..}
    | key == KeyLeft = return (stan{ wcisnieteKlawisze = if keyState == Down then WcisnietyKlawiszLewy : wcisnieteKlawisze else delete WcisnietyKlawiszLewy wcisnieteKlawisze})
    | key == KeyRight = return (stan{ wcisnieteKlawisze = if keyState == Down then WcisnietyKlawiszPrawy : wcisnieteKlawisze else delete WcisnietyKlawiszPrawy wcisnieteKlawisze})
    | otherwise = return stan
obslugaWejscia (EventKey (Char '1') Down _ _) stan@StanGry{..}
    | aktualnyWidok == WyborPoziomu = return (stan{nazwaPlikuZWynikami = nazwaPlikuZWynikami ++ "ZapisWynikow/wyniki1", aktualnyWidok = WyborStopniaTrudnosci})
    | aktualnyWidok == WyborStopniaTrudnosci = return (stan{nazwaPlikuZWynikami = nazwaPlikuZWynikami ++ "latwy.txt", aktualnyWidok = Instrukcja}) 
obslugaWejscia (EventKey (Char '2') Down _ _) stan@StanGry{..}
    | aktualnyWidok == WyborPoziomu = return (stan{nazwaPlikuZWynikami = nazwaPlikuZWynikami ++ "ZapisWynikow/wyniki2", aktualnyWidok = WyborStopniaTrudnosci})
    | aktualnyWidok == WyborStopniaTrudnosci = return (stan{nazwaPlikuZWynikami = nazwaPlikuZWynikami ++ "sredni.txt", aktualnyWidok = Instrukcja})  
obslugaWejscia (EventKey (Char '3') Down _ _) stan@StanGry{..}
    | aktualnyWidok == WyborPoziomu = return (stan{nazwaPlikuZWynikami = nazwaPlikuZWynikami ++ "ZapisWynikow/wyniki3", aktualnyWidok = WyborStopniaTrudnosci})
    | aktualnyWidok == WyborStopniaTrudnosci = return (stan{nazwaPlikuZWynikami = nazwaPlikuZWynikami ++ "trudny.txt", aktualnyWidok = Instrukcja})     
obslugaWejscia (EventKey (Char 'p') Down _ _ ) stan@StanGry{..}
    | aktualnyWidok == Poziom = return stan {aktualnyWidok = Pauza} 
    | aktualnyWidok == Pauza = return stan {aktualnyWidok = Poziom}
    | aktualnyWidok == WprowadzanieNicku  && length nickGracza < 10 = return stan{nickGracza = nickGracza ++ "p"} 
obslugaWejscia (EventKey (Char 'm') Down _ _ ) stan@StanGry{..}
    | aktualnyWidok == Poziom || aktualnyWidok == Pauza || aktualnyWidok == KomunikatOWygranej || aktualnyWidok == KomunikatOPrzegranej = return $ stanWejsciaDoMenuKolejnyRaz stan
    | aktualnyWidok == WprowadzanieNicku  && length nickGracza < 10 = return stan{nickGracza = nickGracza ++ "m"} 
obslugaWejscia (EventKey (Char 'r') Down _ _ ) stan@StanGry{..}
    | aktualnyWidok == Poziom|| aktualnyWidok == Pauza || aktualnyWidok == KomunikatOWygranej || aktualnyWidok == KomunikatOPrzegranej = stanZrestartowanejGry stan
    | aktualnyWidok == WprowadzanieNicku  && length nickGracza < 10 = return stan{nickGracza = nickGracza ++ "r"} 
obslugaWejscia (EventKey (Char c) Down _ _ ) stan@StanGry{..}
    | aktualnyWidok == WprowadzanieNicku  && length nickGracza < 10 = return stan{nickGracza = nickGracza ++ [c]}   
obslugaWejscia _ stan = return stan

-- uruchomienie gry
gra :: IO ()
gra = do
    tloObrazMaybe <- loadJuicyJPG "background4.jpg"
    let Just tloObraz = tloObrazMaybe
    tloMenuMaybe <- loadJuicyJPG "menu.jpg"
    let Just tloMenu = tloMenuMaybe
    playIO trybWyswietlania kolorTla fps stanWejsciaDoMenuPierwszyRaz (narysujGre tloObraz tloMenu) obslugaWejscia iteracja


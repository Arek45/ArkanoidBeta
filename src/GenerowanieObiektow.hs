{-# LANGUAGE RecordWildCards #-}
module GenerowanieObiektow where

import Graphics.Gloss.Interface.Pure.Game

import Stale
import TypyDanych

kolorKlocka :: Klocek -> Color
kolorKlocka Klocek{..} = case liczbaPozostalychUderzen of
    3 -> violet
    2 -> blue
    1 -> cyan
    _ -> white 
kolorKlocka BrakKlocka = white

stworzKlocek :: Klocek -> Picture
stworzKlocek k@Klocek{..} = color (kolorKlocka k) $ uncurry translate pozycja (uncurry rectangleSolid rozmiar)
stworzKlocek _ = Blank

stworzRzadKlockow :: RzadKlockow -> Picture
stworzRzadKlockow rzad = pictures $ map stworzKlocek rzad

stworzPlansze :: PlanszaKlockow -> Picture
stworzPlansze PlanszaKlockow{..} = pictures $ map stworzRzadKlockow klocki

--funkcja rysujaca plansze - TYLKO DO TESTOW
generujRzadKlockow :: Int -> Float -> [Klocek]
generujRzadKlockow liczbaUderzen y =
    [Klocek (x, y) (szerokoscKlocka, wysokoscKlocka) liczbaUderzen | x <- [150]]
--    [Klocek (x, y) (szerokoscKlocka, wysokoscKlocka) liczbaUderzen | x <- [-150, -90 .. 150]]

generujPoziom :: Int -> PlanszaKlockow
--generujPoziom 1 = PlanszaKlockow (map (generujRzadKlockow 1) [100.0]) BrakUderzenia
generujPoziom 1 = PlanszaKlockow
                  [[Klocek (-150, 80) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (-90, 80.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (-30, 80.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (30, 80.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (90, 80.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (150, 80.0) (szerokoscKlocka, wysokoscKlocka) 3
                  ],
                  [Klocek (-150, 120) (szerokoscKlocka, wysokoscKlocka) 2,
                   Klocek (-90, 120.0) (szerokoscKlocka, wysokoscKlocka) 2,
                   Klocek (-30, 120.0) (szerokoscKlocka, wysokoscKlocka) 2,
                   Klocek (30, 120.0) (szerokoscKlocka, wysokoscKlocka) 2,
                   Klocek (90, 120.0) (szerokoscKlocka, wysokoscKlocka) 2,
                   Klocek (150, 120.0) (szerokoscKlocka, wysokoscKlocka) 2
                  ],
                  [Klocek (-150, 160) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-90, 160.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-30, 160.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (30, 160.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (90, 160.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (150, 160.0) (szerokoscKlocka, wysokoscKlocka) 1
                  ],
                  [Klocek (-150, 200) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-90, 200.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-30, 200.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (30, 200.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (90, 200.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (150, 200.0) (szerokoscKlocka, wysokoscKlocka) 1
                  ]] BrakUderzenia
generujPoziom 2 = PlanszaKlockow
                  [[Klocek (-150, 50.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-30, 50.0) (szerokoscKlocka, wysokoscKlocka) 2,
                   Klocek (90, 50.0) (szerokoscKlocka, wysokoscKlocka) 3
                  ],
                  [Klocek (-90, 80.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (30, 80.0) (szerokoscKlocka, wysokoscKlocka) 2,
                   Klocek (150, 80.0) (szerokoscKlocka, wysokoscKlocka) 3
                  ],
                  [Klocek (-150, 110) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-30, 110.0) (szerokoscKlocka, wysokoscKlocka) 2,
                   Klocek (90, 110.0) (szerokoscKlocka, wysokoscKlocka) 3
                  ],
                   [Klocek (-90, 140.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (30, 140.0) (szerokoscKlocka, wysokoscKlocka) 2,
                   Klocek (150, 140.0) (szerokoscKlocka, wysokoscKlocka) 3
                  ],
                  [Klocek (-150, 170) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-30, 170.0) (szerokoscKlocka, wysokoscKlocka) 2,
                   Klocek (90, 170.0) (szerokoscKlocka, wysokoscKlocka) 3
                  ],
                   [Klocek (-90, 200.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (30, 200.0) (szerokoscKlocka, wysokoscKlocka) 2,
                   Klocek (150, 200.0) (szerokoscKlocka, wysokoscKlocka) 3
                  ],
                   [Klocek (-150, 230) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-30, 230.0) (szerokoscKlocka, wysokoscKlocka) 2,
                   Klocek (90, 230.0) (szerokoscKlocka, wysokoscKlocka) 3
                  ]] BrakUderzenia
generujPoziom 3 = PlanszaKlockow
                  [[Klocek (-150, 50) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-90, 50.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-30, 50.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (30, 50.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (90, 50.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (150, 50.0) (szerokoscKlocka, wysokoscKlocka) 1
                  ],
                  [Klocek (-150, 80) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (-90, 80.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (-30, 80.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (30, 80.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (90, 80.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (150, 80.0) (szerokoscKlocka, wysokoscKlocka) 3
                  ],
                  [Klocek (-30, 110.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (30, 110.0) (szerokoscKlocka, wysokoscKlocka) 1
                  ],
                  [Klocek (-150, 140) (szerokoscKlocka, wysokoscKlocka) 2,
                   Klocek (-90, 140.0) (szerokoscKlocka, wysokoscKlocka) 2,
                   Klocek (-30, 140.0) (szerokoscKlocka, wysokoscKlocka) 2,
                   Klocek (30, 140.0) (szerokoscKlocka, wysokoscKlocka) 2,
                   Klocek (90, 140.0) (szerokoscKlocka, wysokoscKlocka) 2,
                   Klocek (150, 140.0) (szerokoscKlocka, wysokoscKlocka) 2
                  ],
                  [Klocek (-30, 170.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (30, 170.0) (szerokoscKlocka, wysokoscKlocka) 1
                  ],
                  [Klocek (-150, 200) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (-90, 200.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (-30, 200.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (30, 200.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (90, 200.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (150, 200.0) (szerokoscKlocka, wysokoscKlocka) 3
                  ],
                  [Klocek (-150, 230) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-90, 230.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-30, 230.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (30, 230.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (90, 230.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (150, 230.0) (szerokoscKlocka, wysokoscKlocka) 1
                  ]] BrakUderzenia
generujPoziom _ = error "Nieznany poziom"

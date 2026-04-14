-- 1. Egy [(String, Int)] típusú lista eleme egy városnevet és a megfelelő népesség
-- értéket tárolja. Írjunk egy Haskell függvényt, amely meghatározza, azokat a
-- városokat, amelyek népesség értéke egy adott n értéknél nagyobb. A kapott
-- városneveket ábécé sorrendbe rendezve külön sorba írjuk ki a képernyőre.
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use notElem" -}
import Data.List(sort, elemIndex)
import Data.Char (isDigit)
valogat varosok n = sort [nev | (nev, szam) <- varosok, szam > n]
fel1 = do
    let varosok = [("sepsiszentgyorgy", 54000), 
            ("kolozsvár", 330000),
            ("marosvasarhely", 130000),
            ("temesvar", 310000), 
            ("arad", 160000),
            ("gyergyoszentmiklos", 18000), 
            ("nagyvarad",196000)
            ]
    let n = 150000
    let varosNevek = valogat varosok n
    --putStr(unlines(varosNevek))
    if null varosNevek
        then do
            putStrLn("Nincs "++ show n ++" erteknel nagyobb lakossagu vatos.")
        else do
            putStrLn("A(z) "++ show n ++ " lakossagnal nagyobb varosok:")
            --putStr(unlines(varosNevek))
            mapM_(\v -> putStrLn ("- " ++ v)) varosNevek

-- 2. Írjunk egy Haskell függvényt, amely meghatározza egy bemeneti egész
-- számokat tartalmazó lista azon elemeit, amelyek nem tartalmazzák a 0
-- számjegyet. Az eredmény számokat szóközzel elválasztva írjuk ki a
-- képernyőre.
-- Például:
-- ● Bemenet: [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
-- ● Kimenet: A 0 szamjegyet nem tartalmazo szamok a kovetkezok: 3223 816252
-- 23561 61
-- ● Amennyiben nincsenek ilyen számok, a kimenet a következő: “Nincsenek
-- olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet.”

nincsnulla n = not('0' `elem` show n)
fel2 = do
    let szamok = [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
    let eredmeny = [show x | x <- szamok , nincsnulla x]
    if null eredmeny
        then 
            putStrLn("Nincsenek olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet.")
        else do 
            putStr("A nulla szamjegyet nem tartalmazo szamok a kovetkezok: ")
            putStrLn(unwords(eredmeny))

-- 3. Egy listában karakterláncok vannak, írjunk egy Haskell programot, amely kiírja
-- azokat a karakterláncokat a képernyőre egymás alá rendezve ábécé
-- sorrendbe, amelyekben nincsenek számjegyek.


vanESzamjegy szo = not (any isDigit szo)
fel3 = do
    let szavak = ["2023tuple", "function", "float", "higher-order", "variable10", "may13be", "0recursion", "monad", "class"]
    let eredmeny = [szo | szo <- szavak ,   vanESzamjegy szo]
    if null eredmeny
        then 
            putStrLn("Nincsenek olyan karakterlancok, amelyek nem tartalmaznak szamot.")
        else do
            putStr("A szavak amiben nincs szam a kovetkezok: ")
            putStrLn(unwords(sort(eredmeny)))
            
-- 4. Írjunk egy Haskell programot, amely meghatározza, hogy az s karakterláncnak
-- melyek a szomszédjai az lsS karakterláncokat tartalmazó listából, ahol egy
-- karakterlánc szomszédjait az ábécé sorrend szerinti kell érteni.
-- Például:
-- ● Bemenet:
-- s = feri
-- lsS = Mari Zsuzsa szidi Lori kata feri teri Dani zsolti
-- ● Kimenet: feri baloldali szomszedja Zsuzsa, jobboldali szomszedja pedig kata
szomszedok s lss = do
    let rendezett = sort lss
    -- Megkeressük a 'feri' indexét a rendezett listában
    case elemIndex s rendezett of
        Nothing -> putStrLn "A megadott nev nincs a listaban."
        Just i  -> do
            let balSzomszed = if i > 0 
                              then rendezett !! (i - 1) 
                              else "nincs"
            let jobbSzomszed = if i < length rendezett - 1 
                               then rendezett !! (i + 1) 
                               else "nincs"
            
            putStrLn (s ++ " baloldali szomszedja " ++ balSzomszed ++ 
                      ", jobboldali szomszedja pedig " ++ jobbSzomszed)
                
fel4 = do
    let emberek=["Mari", "Zsuzsa", "szidi", "Lori", "kata", "feri", "teri", "Dani", "zsolti"]
    let x ="feri"
    szomszedok x emberek


-- 5. Egy [(String, Int, Int)] típusú lista eleme egy telefon márkanevet, egy eladási
-- értéket, és egy árat tartalmaz. Írjunk egy Haskell programot, amely
-- meghatározza azokat a telefonokat, amelyekből a legtöbbet adtak el, illetve
-- mennyi volt ez az érték. Az eredmény márkaneveket rendezve egymás alá
-- írjuk, amelyek elé írjuk ki egy kisérő szöveggel együtt a maximális eladási
-- értéket.
-- Például:
-- ● Bemenet: [("iphoneS1", 20, 2500), ("huaweiS1", 30, 1700), ("huaweiS2", 25,
-- 3100), ("samsungA1", 30, 2000), ("nokia", 10, 1900), ("iphoneS2", 10, 2200),
-- ("samsungA2", 15, 1650), ("iphone3", 30, 1800)]
-- ● Kimenet: A maximalis eladasi ertek 30. A telefonok, amelyeknek ennyi az
-- eladasi erteke a kovetkezok:
-- - iphone3
-- - huaweiS1
-- - samsungA1

maxEladas lista = maximum[eladas | (telefon, eladas, ar)<-lista]
fel5 = do
    let lista = [("iphoneS1", 20, 2500), ("huaweiS1", 30, 1700), ("huaweiS2", 25, 3100), ("samsungA1", 30, 2000), ("nokia", 10, 1900), ("iphoneS2", 10, 2200), ("samsungA2", 15, 1650), ("iphone3", 30, 1800)]
    let maxi=maxEladas lista
    let eredmeny = [telefon | (telefon, eladas, ar) <- lista, eladas == maxi]
    putStrLn("A maximalis eladasi ertek a " ++ show maxi ++ ". A telefonok amelyeknek ennyi az eladasi erteke a kovetkezok: ")
    mapM_ (\v -> putStrLn ("- " ++ v)) eredmeny

-- 6.Írj egy Haskell függvényt, melynek egy lista a bemenete, és megadja azokat a
-- számokat, amelyek előfordulási száma páratlan. Az eredményt írasd ki a
-- példában szereplő formában, előfordulási érték szerint rendezve.
-- Például:
-- ● Bemenet: [7]
-- ● Kimenet: Elofordulas: 1 -> Ertek: 7
-- ● Bemenet: [1, 1, 2]
-- ● Kimenet: Elofordulas: 1 -> Ertek: 2
-- ● Bemenet: [1, 1]
-- ● Kimenet: Nincs paratlan elofordulasi ertekkel rendelkezo szam.
-- ● Bemenet: [1, 1, 2, 3, 4, 2, 6, 2, 4, 4, 2, 6, 7, 6, 6, 2]
-- ● Kimenet:
-- Elofordulas: 1 -> Ertek: 3
-- Elofordulas: 1 -> Ertek: 7
-- Elofordulas: 3 -> Ertek: 4
-- Elofordulas: 5 -> Ertek: 2
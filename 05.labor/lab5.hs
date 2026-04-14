-- # 5. labor

import Prelude hiding (splitAt, notElem, concat, repeat, replicate, cycle, iterate, any, all,
                       length, sum, elem, reverse, product, maximum, (++), map, filter)
import Distribution.Simple.Setup (trueArg)
import GHC.Exts.Heap (GenClosure(key))
import Data.List ((++))

-- I. Írjuk meg a beépített splitAt, notElem, concat, repeat, replicate, cycle, iterate, any, all függvényeket.

splitAt n ls
    | n <= 0    = ([], ls)
    | otherwise = seged n ls
  where
    seged 0 xs     = ([], xs)
    seged _ []     = ([], [])
    seged k (x:xs) = let (ys, zs) = seged (k-1) xs in (x:ys, zs)

notElem _ []     = True
notElem e (x:xs)
    | e == x     = False
    | otherwise  = notElem e xs

concat []           = []
concat (xs:xss)     = xs ++ concat xss

repeat x = x : repeat x

replicate 0 _ = []
replicate n x
    | n > 0     = x : replicate (n-1) x
    | otherwise = []

cycle []     = []
cycle xs     = xs ++ cycle xs

iterate f x = x : iterate f (f x)

any _ []     = False
any p (x:xs)
    | p x        = True
    | otherwise  = any p xs

all _ []     = True
all p (x:xs)
    | p x        = all p xs
    | otherwise  = False

-- II. Írjunk Haskell-függvényt, amely a foldl vagy a foldr függvényt alkalmazva

-- - implementálja a length, sum, elem, reverse, product, maximum, insert-sort, ++, map, filter függvényeket,

myLengthL ls = foldl op 0 ls
    where
        op res k = res+1

myLengthR ls = foldr op 0 ls
    where 
        op k res = res + 1

mySumL ls = foldl op 0 ls
    where
        op res k = res + k

mySumR ls = foldr op 0 ls
    where
        op k res = res + k

myElemL c ls = foldl (op c) False ls
    where
        op c res k
            | c== k =True
            |otherwise = res

myElemR c ls = foldr (op c) False ls
    where
        op c k res
            | c== k =True
            |otherwise = res
myReverseL ls = foldl op [] ls
    where 
        op res k = k : res

myReverseR ls = foldr op [] ls
    where 
        op k res = res ++ [k]
myProductL ls = foldl op 1 ls
    where
        op k res = k*res 

myProductR ls = foldr op 1 ls
    where
        op res k = k*res 
myMaximumR ls = foldr op (head ls) ls
    where
        op res k 
            | k>res=k
            |otherwise = res

myMaximumL ls = foldl op (head ls) ls
    where
        op k res
            | k>res=k
            |otherwise = res


ins :: (Ord a) => a -> [a] -> [a]
ins x [] = [x]
ins x (k : ve)
    | x > k = k : ins x ve
    | otherwise = x : k : ve

myAppend ls1 ls2 = foldr op ls2 ls1
    where 
        op k res = k : res

myMap fg ls = foldr (op fg) [] ls
    where 
        op fg k res = fg k : res


myFilter fg ls = foldr (op fg) [] ls
    where
        op fg k res
            | fg k == True = k : res
            | otherwise = res

mySumPos ls = foldl op 0
    where 
        op res k
            | k > 0 = res + k
            | otherwise = res


myProduct ls = foldl op 1 ls
    where 
        op res k 
            -- | mod k 2 == 0 = res * k
            | even k = res * k
            | otherwise = res

-- - n-ig a négyzetszámokat.


-- - meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét: $$a_0 + x_0(a_1 + x_0(a_2 + x_0(a_3 + \ldots + x_0(a_{n-1}+ x_0 \cdot a_n))))$$

polinom coeffs x0 = foldl (\acc a -> acc * x0 + a) 0 coeffs

-- III.

-- - Írjunk egy Haskell-függvényt, amely egy String típusú listából meghatározza azokat a szavakat, amelyek karakterszáma a legkisebb. Például ha a lista a következő szavakat tartalmazza:  function class Float higher-order monad tuple variable Maybe recursion  akkor az eredmény-lista a következőkből áll: class Float monad tuple Maybe
-- - Írjunk egy talalat Haskell-függvényt, amely meghatározza azt a listát, amely a bemeneti listában megkeresi egy megadott elem előfordulási pozícióit.
--   Például a következő függvényhívások esetében az első az 5-ös előfordulási pozícióit, míg a második az e előfordulási pozícióinak listáját határozza meg.

--   ```haskell
--   > talalat 5 [3, 13, 5, 6, 7, 12, 5, 8, 5]
--   [2, 6, 8]
--   > talalat 'e' "Bigeri-vizeses"
--   [3,10,12]
--   ```
-- - Írjunk egy osszegT Haskell-függvényt, amely meghatározza egy (String, Int)értékpárokból álló lista esetében az értékpárok második elemeiből képzett összeget.
--   Például:

--   ```haskell
--   > ls = [("golya",120),("fecske",85),("cinege",132)]
--   > osszegT ls
--   337
--   ```
-- - Írjunk egy atlagTu Haskell-függvényt, amely egy kételemű, tuple elemtípusú lista esetében átlagértékeket számol a második elem szerepét betöltő listaelemeken. Az eredmény egy tuple elemtípusú lista legyen, amelynek kiíratása során a tuple-elemeket formázzuk, és külön sorba írjuk őket.
--   Például:

--   ```haskell
--   > :set +m
--   > ls = [("mari",[10, 6, 5.5, 8]), ("feri",[8.5, 9.5]),
--   | ("zsuzsa",[4.5, 7.9, 10]),("levi", [8.5, 9.5, 10, 7.5])]
--   > atlagTu ls
--   mari 7.375
--   feri 9.0
--   zsuzsa 7.466666666666666
--   levi 8.875
--   ```
-- legrovidebbek [] = []
-- legrovidebbek xs = [s | s <- xs, length s == minHossz]
--   where
--     minHossz = minimum (map length xs)
-- talalat x xs = [i | (i, e) <- zip [0..] xs, e == x]
-- osszegT xs = sum [n | (_, n) <- xs]
-- atlagTu xs = mapM_ kiir xs
--   where
--     kiir (nev, szamok) = putStrLn (nev ++ " " ++ show (atlag szamok))
--     atlag ys = sum ys / fromIntegral (length ys)

-- # 5. labor

import Prelude hiding (splitAt, notElem, concat, repeat, replicate, cycle, iterate, any, all,
                       length, sum, elem, reverse, product, maximum, (++), map, filter)

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

length xs = foldr (\_ acc -> acc + 1) 0 xs

sum xs = foldl (+) 0 xs

elem e xs = foldr (\x acc -> acc || (x == e)) False xs

reverse xs = foldl (\acc x -> x : acc) [] xs

product xs = foldl (*) 1 xs

maximum []     = error "üres lista"
maximum (x:xs) = foldl max x xs

insertSort ls = foldr insert [] ls
  where
    insert x []     = [x]
    insert x (y:ys)
        | x <= y    = x : y : ys
        | otherwise = y : insert x ys

xs ++ ys = foldr (:) ys xs

map f xs = foldr (\x acc -> f x : acc) [] xs

filter p xs = foldr (\x acc -> if p x then x : acc else acc) [] xs

-- - meghatározza egy lista pozitív elemeinek összegét,

pozitivOsszeg xs = foldr (\x acc -> if x > 0 then x + acc else acc) 0 xs

-- - egy lista páros elemeinek szorzatát,

parosSzorzat xs = foldr (\x acc -> if even x then x * acc else acc) 1 xs

-- - n-ig a négyzetszámokat.

negyzetek n = map (\x -> x * x) [1 .. n]

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

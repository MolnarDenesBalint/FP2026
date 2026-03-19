-- # 4. labor


-- I. Definiáljuk azt a Haskell-listát, amely tartalmazza:
-- - az első n páros szám négyzetét,
parosNegyzet n = [ (2*x)^2 | x <- [1..n] ]
-- - az első [1, 2, 2, 3, 3, 3, 4, 4, 4, 4,...],

szamokLs n 
    | n/=0 = szamokLs (n-1) ++  replicate n n 
    |otherwise = replicate n n 

szamokLs2 n i 
    | i < n     = replicate i i ++ szamokLs2 n (i+1)
    | i == n    = replicate i i
    | otherwise = []  



-- - az első [2, 4, 4, 6, 6, 6, 8, 8, 8, 8...],

szamokLs3 n i 
    | i < n     = replicate i (i*2) ++ szamokLs3 n (i+1)
    | i == n    = replicate i (i*2)
    | otherwise = []

-- - az első [n, n-1, ..., 2, 1, 1, 2, ..., n-1, n],

szamokLs4 n = [n,n-1 .. 1 ] ++ [1..n]
szamokLs5 n = reverse [1 .. n]  ++ [1..n] 

-- - váltakozva tartalmazzon True és False értékeket,

tfLs n  = take n ls 
    where
        ls = [True, False] ++ ls 

-- - váltakozva tartalmazza a 0, 1, -1 értékeket.
valtakozo01 = 0 : 1 : (-1) : valtakozo01



-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely


-- - meghatározza egy adott szám osztóinak számát,

osztok n = [i | i <- [1.. n ] , mod n i ==  0 ]

osztokSzama n = length $ osztok n 

osztokSzama3 n = foldl (\res i -> if n `mod` i == 0 then res + 1 else res) 0 [1..n]

-- - meghatározza egy adott szám legnagyobb páratlan osztóját,

maxParatlanOSzto n = last (filter odd (osztok n))

maxParatlanOszto2 n = [i | i <- [1,3 .. n ], mod n i == 0 ]

maxParatlanOSzto3 n = foldl(\res i -> if mod n i == 0 then i else res ) 1 [1,3 .. n ]

-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,


decPszam x p
    | x < p     = 1                     
    | otherwise = 1 + decPszam (x `div` p) p  

decP 0 _ = [0]
decP x p 
    | x < p     = [x]
    | otherwise = decP (x `div` p) p ++ [x `mod` p]


-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjegy,

decPMax2 x p = myMaximum $ decP x p 
    where
        myMaximum [n ] = n 
        myMaximum (n1 : n2 : ls )
            |n1 > n2 = myMaximum (n1 : ls)
            |otherwise = myMaximum (n2 : ls )

-- - meghatározza az a és b közötti Fibonacci számokat, a > 50.

fibo = fiboSg 0 1 0
    where
        fiboSg a b res = res :  fiboSg b res (res+b)

-- III. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely


-- - meghatározza egy lista pozitív elemeinek átlagát,

pozAtlag ls = if db == 0 then 0 else szumma / fromIntegral db
  where
    (szumma, db) = szamol ls
    szamol [] = (0, 0)
    szamol (x:xs) 
        | x > 0     = (x + s, 1 + d)
        | otherwise = (s, d)
        where (s, d) = szamol xs 

-- - meghatározzuk azt a listát, amely tartalmazza az eredeti lista minden n-ik elemét,

listaN ls n = seged ls n
  where
    seged [] _ = []
    seged (x:xs) 1 = x : seged xs n
    seged (x:xs) k = seged xs (k-1)
-- - tükrözi egy lista elemeit,
tukroz [] = []
tukroz (x:xs) = tukroz xs ++ [x]
-- - két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy hogy a lista elemeit csak egyszer járja be,
maxPoziciok2 ls = indexek ls (keresMax ls) 0
  where
    keresMax [x] = x
    keresMax (x:xs) = let m = keresMax xs in if x > m then x else m
    
    indexek [] _ _ = []
    indexek (x:xs) m i 
        | x == m    = i : indexek xs m (i+1)
        | otherwise = indexek xs m (i+1)
-- - meghatározza egy lista leggyakrabban előforduló elemét.
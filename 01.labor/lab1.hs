import System.Win32 (LOCALESIGNATURE(lsCsbDefault))
osszeg :: Int -> Int -> Int
kulombseg :: Int -> Int -> Int
szorzat :: Int -> Int -> Int
hanyados ::  Float -> Float ->Float
maradek :: Int -> Int -> Int
osszeg a b = a + b
kulombseg a b = a - b
szorzat a b = a * b
hanyados a b = a / b
maradek a b = a `mod` b

elsoF a b = (-b)/a
abszolut a
    |a < 0 = -a
    | otherwise =a
abszolut2 a = if a<0 then -a else a
elojel a = if a<0 then "-" else  if a>0 then "+" else "0"
elojel2 a
    | a<0 = "negativ"
    | a>0 = "pozitiv"
    | otherwise = "nulla"
max1 a b = if a<b then b else a
min1 a b = if a>b then b else a
--II resz.
negyzetgyok n = [ sqrt(i) | i <- [1 .. n] ]
negyzetszam n = [ i ^ 2 | i <- [1 .. n] ]
kob n =[ i ^ 3 | i <- [1 .. n] ]
nemnegyzet n = [j | j <- [1 .. n], (sqrt(j) * sqrt(j) /= j)]
hatvany x n = [x^i | i <- [1 .. n]]
parososztok n=[i | i <- [2, 4 .. n   ], (n `mod` i == 0)]

osztok n=[i | i <- [1 .. n   ], (n `mod` i == 0)]
primek n=[i | i <- [2 .. n], length(osztok i)==2]
isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | otherwise = null [x | x <- [2..limit], n `mod` x == 0]
    where limit = floor (sqrt (fromIntegral n))
osszetettt n=[i | i<- [0 .. n], isPrime i /= True]
paratlanosszetett n = [i | i<- [1, 3 .. n], isPrime i /= True]  
pitagorasz n = [(a,b,c) | c<-[1..n], b <- [1..c], a <-[1..b], c^2==a^2 + b^2]
betuszam = zip ['a'..  'z'][0 .. 25]
parok n= [(a,b) | b<-[n, n-1..0], a<-[0 .. n], a+b==n ]
tfls n = take n ls
    where
        ls= [True, False] ++ ls



main :: IO()
main = do
    putStrLn("10-ig primek:")
    print (primek 10)
    putStrLn("100-ig osszetett szamok:")
    print (osszetettt 100)
    putStrLn("100-ig osszetett paratlan szamok:")
    print (paratlanosszetett 100)
    putStrLn("100-ig pitagoraszi szamok:")
    print (pitagorasz 100)
    putStrLn("Betuszam parok:")
    print (betuszam)
    putStrLn("Szamparok:")
    print (parok 5)
    print (tfls 5)

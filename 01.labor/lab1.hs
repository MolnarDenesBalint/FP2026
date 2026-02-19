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
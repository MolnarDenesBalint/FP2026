
  szOsszeg 0 = 0
  szOsszeg x = ( x `mod` 10 ) + szOsszeg (x `div` 10)


  szSzorzat 1 = 1
  szSzorzat 0 = 0
  szSzorzat x = ( x `mod` 10 ) * szSzorzat (x `div` 10)

  szSzam 0 = 0
  szSzam x = 1+ szSzam (x `div` 10)


  fugv4 :: Int -> Int -> Int
  fugv4 0 _ = 0
  fugv4 n d
      | utolso == d = d + fugv4 maradek d
      | otherwise   = fugv4 maradek d
      where
          utolso  = abs n `mod` 10
          maradek = abs n `div` 10

  parossz 0 = 0
  parossz n
        |(n `mod` 10) `mod` 2 == 0 = 1+parossz (n `div`10)
        | otherwise = parossz (n `div`10 )

  legnagyobbsz 0 = 0
  legnagyobbsz n
        | n `mod`10> legnagyobbsz (n `div` 10) = n `mod` 10
        | otherwise = (n `mod` 10)

  fugv :: Integral a => a -> a -> a -> Int
  fugv n b d
    | n == 0    = if d == 0 then 1 else 0
    | n < 0     = fugv (-n) b d
    | otherwise = szamol n
   where
    szamol 0 = 0
    szamol x = (if x `rem` b == d then 1 else 0) + szamol (x `div` b)

  fib :: Int -> Integer
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n - 1) + fib (n - 2)
  fiboN n = fibo 0 1 0 n
    where
      fibo _ _ res 0 = res
      fibo a b res n1 = fibo b res (b+ res) (n1-1)

  main :: IO()
  main = do
    putStrLn "12345 szamjegyek osszege"
    print (szOsszeg 12345)
    putStrLn "12345 szamjegyek szorzata"
    print (szSzorzat 12345)
    putStrLn "12345 szamjegyek szama"
    print (szSzam 12345)
    putStrLn "125345 5 szamjegyek szama"
    print (fugv4 125345 5)
    putStrLn "12345 paros szamjegyek szama"
    print (parossz 12345 )
    putStrLn "123459678 legnagyobb szamjegye"
    print (legnagyobbsz 1239459678 )
    putStrLn "7673573 10es szrben a 7 szamjegyek szama"
    print (fugv 7673573 10 7 )
    putStrLn "1023 2es szrben a 1 szamjegyek szama"
    print (fugv 1023 2 1 )
    putStrLn "1000ik fibonacci szam"
    print (fiboN 1000 )




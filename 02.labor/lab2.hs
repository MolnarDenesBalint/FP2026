
  szOsszeg 0 = 0
  szOsszeg x = ( x `mod` 10 ) + szOsszeg (x `div` 10)

  
  szSzorzat 1 = 1
  szSzorzat 0 = 0
  szSzorzat x = ( x `mod` 10 ) * szSzorzat (x `div` 10) 

  szSzam 0 = 0
  szSzam x = 1+ szSzam(x `div` 10)


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

  main :: IO()
  main = do
    putStrLn("12345 szamjegyek osszege")
    print (szOsszeg 12345)
    putStrLn("12345 szamjegyek szorzata")
    print (szSzorzat 12345)
    putStrLn("12345 szamjegyek szama")
    print (szSzam 12345)
    putStrLn("125345 5 szamjegyek szama")
    print (fugv4 125345 5)
    putStrLn("12345 paros szamjegyek szama")
    print (parossz 12345 )


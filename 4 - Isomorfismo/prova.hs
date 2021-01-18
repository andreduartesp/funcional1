-- Mostre, em Haskell, que 3*a = a + a + a são ismórficos.


-- (3, a) = Either (Either (a a)) a

-- ida
-- (3, a) -> Either (Either (a a)) a

ida :: (Int -> a) -> a
ida n a
   | n == 0 = 0
   | otherwise = a `either` ida ((n-1) a)

-- volta
-- Either (Either (a a)) a -> (3,a)

-- volta (ida x) = x

-- ida (volta x) = x
-- (Int, a) = Either (Either (a a)) a
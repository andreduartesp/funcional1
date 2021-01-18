module Cap2 where

e21a :: [Int]
e21a = map (^ 11) [0 .. 6]

e21b :: [Int]
e21b = filter (\x -> mod x 4 /= 0) [0 .. 39]

e21c :: [[Char]]
e21c = map (\x -> "A" ++ [x] ++ "BB") ['a' .. 'g']

e21e :: [Float]
e21e = map (\x -> 1 / (2 ^ x) ) [0 .. 5]

e21f :: [Int]
e21f = map (\x -> x * 9 + 1) [0 .. 7]

e21g :: [Int]
e21g = filter (\x -> mod x 2 == 0 && notElem x [0,6,14,20,26]) [0..30]

e21h :: [Char]
e21h = filter (\x -> notElem x ['B','F','H', 'I','K']) ['@'..'L']


e24 :: [[Char]] -> [Int]
e24 = filter odd . map length
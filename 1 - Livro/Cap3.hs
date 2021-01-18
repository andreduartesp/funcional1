module Cap3 where

-- 3.1
data Pergunta = Sim | Nao deriving Show

pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0

listPergs :: [Pergunta] -> [Int]
listPergs xs = [pergNum x | x <- xs]

and :: (Pergunta, Pergunta) -> Bool
and (Sim, Sim) = True
and (Sim, Nao) = False
and (Nao, Sim) = False
and (Nao, Nao) = False 

or :: (Pergunta, Pergunta) -> Bool
or (Sim, Sim) = True
or (Sim, Nao) = True
or (Nao, Sim) = True
or (Nao, Nao) = False 

not :: Pergunta -> Bool
not Sim = False
not Nao = True

--3.2
data Temperatura = Celcius | Farenheight | Kelvin deriving Show

converterCelcius :: Double -> Temperatura -> Double
converterCelcius x Celcius = x
converterCelcius x Farenheight = (*) ((-) x 32) ((/) 5 9)
converterCelcius x Kelvin = (-) x 273.15

converterKelvin :: Double -> Temperatura -> Double
converterKelvin x Celcius = (+) x 273.15
converterKelvin x Farenheight = (+) ((*) ((-) x 32) ((/) 5 9)) 273.15
converterKelvin x Kelvin = x

converterFarenheight :: Double -> Temperatura -> Double
converterFarenheight x Celcius = (+) ((*) x ((/) 9 5)) 32
converterFarenheight x Farenheight = x
converterFarenheight x Kelvin = (+) ((*) ((-) x 273.15) ((/) 9 5)) 32

--3.5
data Imperial = In | Yd | Ft

converteMetros :: Double -> Imperial -> Double 
converteMetros x In = (*) x 0.0254
converteMetros x Yd = (*) x 0.9144
converteMetros x Ft = (*) x 0.3048

conventeImperial :: Double -> Imperial -> Double 
conventeImperial x In = (/) x 0.0254
conventeImperial x Yd = (/) x 0.9144
conventeImperial x Ft = (/) x 0.3048

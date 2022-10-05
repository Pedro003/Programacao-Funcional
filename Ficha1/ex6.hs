data Ponto = Cartesiano Double Double 
            | Polar Double Double
            deriving (Show,Eq)  

posx :: Ponto -> Double
posx ponto = case ponto of Cartesiano x _ -> x
                           Polar a b -> a * cos b

posy :: Ponto -> Double
posy ponto = case ponto of Cartesiano _ y -> y
                           Polar a b -> a * sin b 

raio :: Ponto -> Double
raio ponto = case ponto of Cartesiano x y -> sqrt(x^2 + y^2)
                           Polar a _ -> a

angulo :: Ponto -> Double
angulo ponto = case ponto of Cartesiano x y | (x < 0 && y == 0) -> pi
                                            | x < 0 -> pi + atan (y/x) 
                                            | otherwise -> atan (y/x)
                             Polar _ b -> b                 

dist :: Ponto -> Ponto -> Double
dist p1 p2 = sqrt(((posx p1 - posx p2) ^ 2) + (posy p1 - posy p2) ^ 2)
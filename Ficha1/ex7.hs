-- Ponto só com Cartesiano, ou seja diferente do ex6
type Ponto = (Double,Double)

data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show,Eq)

poligono :: Figura -> Bool
poligono figura = case figura of Circulo _ _ -> False
                                 Rectangulo (a,b) (x,y) | a == x && b == y -> False
                                                        | otherwise -> True
                                 Triangulo (a,b) (x,y) (n,m) | (y - b) / (x - a) /= (m - y) / (n - x) -> True
                                                             | otherwise -> False                       

vertices :: Figura -> [Ponto]
vertices figura = case figura of Circulo a _ -> []
                                 Rectangulo x y -> [x,y]
                                 Triangulo x y z -> [x,y,z]

-- incompleto
area :: Figura -> Double
area (Triangulo p1 p2 p3) =
        let a = dist p1 p2
            b = dist p2 p3
            c = dist p3 p1
            s = (a+b+c) / 2 -- semi-perimetro
        in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron 

-- falta
perimetro :: Figura -> Double
        
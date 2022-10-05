data Hora = H Int Int deriving (Show,Eq)


horaValida :: Hora -> Bool
horaValida (H h m) = (h >= 0 && h<24) && (m>=0 && m<60)

-- true qunado a 1a é maior que a 2a
comparaHoras :: Hora -> Hora -> Bool
comparaHoras (H h1 m1) (H h2 m2) = if h1 > h2 then True
                               else if (h1 == h2) then m1 > m2
                                else False
-- outra alternativa a comparaHoras
comparaHorasB :: Hora -> Hora -> Bool
comparaHorasB (H h1 m1) (H h2 m2) | h1 > h2 = True
                              | h1 == h2 = m1 > m2
                              | h1 < h2 = False   

horasParaMin :: Hora -> Int
horasParaMin (H a b) = a*60 + b

minParaHoras :: Int -> Hora
minParaHoras a = (H (div a 60) (mod a 60))

diferencaHoras :: Hora -> Hora -> Int
diferencaHoras (H h1 h2) (H h3 h4) = abs(horasParaMin(H h1 h2) - horasParaMin(H h3 h4))

adicionaMin :: Int -> Hora -> Hora
adicionaMin a (H n m) = minParaHoras((horasParaMin (H n m)) + a)
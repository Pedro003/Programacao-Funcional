-- comentario

-- tudo que tem a ver com tipos começa por letra maiuscula

type Hora = (Int,Int)

meiaNoiteEUmQuarto :: Hora
meiaNoiteEUmQuarto = (0,15)

duasMenosUmQuarto :: Hora
duasMenosUmQuarto = (13,45)

horaValida :: Hora -> Bool
horaValida (h,m) = (h >= 0 && h<24) && (m>=0 && m<60)

-- true qunado a 1a é maior que a 2a
comparaHoras :: Hora -> Hora -> Bool
comparaHoras (h1,m1) (h2,m2) = if h1 > h2 then True
                               else if (h1 == h2) then m1 > m2
                                else False
-- outra alternativa a comparaHoras
comparaHorasB :: Hora -> Hora -> Bool
comparaHorasB (h1,m1) (h2,m2) | h1 > h2 = True
                              | h1 == h2 = m1 > m2
                              | h1 < h2 = False   

horasParaMin :: Hora -> Int
horasParaMin (a,b) = a*60 + b

minParaHoras :: Int -> Hora
minParaHoras a = (div a 60,mod a 60)

diferencaHoras :: Hora -> Hora -> Int
diferencaHoras h1 h2 = abs(horasParaMin(h1) - horasParaMin(h2))

adicionaMin :: Int -> Hora -> Hora
adicionaMin a b = minParaHoras((horasParaMin b) + a)
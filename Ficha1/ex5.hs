data Semaforo = Verde 
              | Amarelo  
              | Vermelho 
              deriving (Show,Eq)

next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde 

stop :: Semaforo -> Bool 
stop Vermelho = True
stop _ = False

safe :: Semaforo -> Semaforo -> Bool
safe Verde Vermelho = True
safe Vermelho Verde = True
safe _ _ = False
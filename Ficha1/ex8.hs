import Data.Char

isLower' :: Char -> Bool
isLower' a = a `elem` ['a'..'z']

isDigit' :: Char -> Bool
isDigit' a = a `elem` ['0'..'9']

isAlpha' :: Char -> Bool 
isAlpha' a = isLower' a || a `elem` ['A'..'Z']

toUpper' :: Char -> Char
toUpper' a | isLower' a = chr (ord a -32)
           | otherwise = a

intToDigit' :: Int -> Char
intToDigit' a = chr (a + 48)      

digitToInt' :: Char -> Int 
digitToInt' a = ord a - 48
type Fila = [Int]

type Tablero = [Fila]

type Posicion = (Int, Int)

type Camino = [Posicion]

maximo :: Tablero -> Int
maximo [] = 0
maximo (x : []) = maximoDeFila x
maximo (x : y : xs)
  | maximoDeFila x > maximoDeFila y = maximo (x : xs)
  | otherwise = maximo (y : xs)

masRepetido :: Tablero -> Int
masRepetido [] = 0
masRepetido [x] = masRepetidoFila x
masRepetido (x : y : xs)
  | vecesRepetido (masRepetidoFila x) x >= vecesRepetido (masRepetidoFila y) y = masRepetido (x:xs)
  | otherwise = masRepetido (y:xs ++ [x])

-- Auxiliares
maximoDeFila :: Fila -> Int
maximoDeFila [] = 0
maximoDeFila (x : []) = x
maximoDeFila (x : y : xs)
  | x > y = maximoDeFila (x : xs)
  | otherwise = maximoDeFila (y : xs)

masRepetidoFila :: Fila -> Int
masRepetidoFila [] = 0
masRepetidoFila [x] = x
masRepetidoFila (x : y : xs)
  | vecesRepetido x (x : y : xs) > vecesRepetido y (x : y : xs) = masRepetidoFila (x : xs)
  | otherwise = masRepetidoFila (y : xs)

vecesRepetido :: Int -> Fila -> Int
vecesRepetido _ [] = 0
vecesRepetido n (x : xs)
  | n == x = 1 + vecesRepetido n xs
  | otherwise = vecesRepetido n xs

compararMasRepFilaConTablero :: Fila -> Tablero -> Bool
compararMasRepFilaConTablero f [] = True
compararMasRepFilaConTablero f (x:xs)
    |vecesRepetido(masRepetidoFila f) f >= vecesRepetido (masRepetidoFila x)x = compararMasRepFilaConTablero f xs
    |otherwise = False
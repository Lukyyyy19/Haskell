import Language.Haskell.TH (Strict)

{-

CONSIGNA

1) Votos en Blanco [1 punto]

problema votosEnBlanco (formulas: seq⟨String x String⟩,votos:seq< Z >, cantTotalVotos: Z) : Z {
  requiere: {formulasValidas(formulas)}
  requiere: {|formulas| = |votos|}
  requiere: {Todos los elementos de votos son mayores o iguales a 0}
  requiere: {La suma de todos los elementos de votos es menor o igual a cantTotalVotos}
  asegura: {res es la cantidad de votos emitidos que no correspondieron a niguna de las fórmulas que se presentaron}
}

2) Formulas Válidas [3 puntos]

problema formulasValidas (formulas: seq⟨String x String⟩) : Bool {
  requiere: {True}
  asegura: {(res = true) <=> formulas no contiene nombres repetidos, es decir que cada candidato está en una única fórmula (no se puede ser candidato a presidente y a vicepresidente ni en la misma fórmula ni en fórmulas distintas)}
}

3) Porcentaje de Votos [3 puntos]

problema porcentajeDeVotos (presidente: String, formulas: seq⟨String x String⟩,votos:seq< Z >) : R {
  requiere: {La primera componente de algún elemento de formulas es presidente}
  requiere: {formulasValidas(formulas)}
  requiere: {|formulas| = |votos|}
  requiere: {Todos los elementos de votos son mayores o iguales a 0}
  requiere: {Hay al menos un elemento de votos que es mayor que estricto que 0}
  asegura: {res es el porcentaje de votos que obtuvo la fórmula encabezada por presidente sobre el total de votos afirmativos}
}
Para resolver este ejercicio pueden utilizar la siguiente función que devuelve como Float la división entre dos números de tipo Int:

division :: Int -> Int -> Float
division a b = (fromIntegral a) / (fromIntegral b) 

4) Próximo Presidente [3 puntos]

problema proximoPresidente (formulas: seq⟨String x String⟩, votos:seq< Z >) : String {
  requiere: {formulasValidas(formulas)}
  requiere: {|formulas| = |votos|}
  requiere: {Todos los elementos de votos son mayores o iguales a 0}
  requiere: {Hay al menos un elemento de votos mayores estricto a 0}
  requiere: {|formulas| > 0}
  asegura: {res es el candidato a presidente de formulas más votado de acuerdo a los votos contabilizados en votos}
}

-}



-- 1) Votos en Blanco [1 punto]

-- problema votosEnBlanco (formulas: seq⟨String x String⟩,votos:seq< Z >, cantTotalVotos: Z) : Z {
--   requiere: {formulasValidas(formulas)}
--   requiere: {|formulas| = |votos|}
--   requiere: {Todos los elementos de votos son mayores o iguales a 0}
--   requiere: {La suma de todos los elementos de votos es menor o igual a cantTotalVotos}
--   asegura: {res es la cantidad de votos emitidos que no correspondieron a niguna de las fórmulas que se presentaron}
-- }

votosEnBlanco :: [(String,String)] -> [Int] -> Int -> Int
votosEnBlanco _ xs tot = tot - sumaDeLista xs

sumaDeLista :: [Int] -> Int
sumaDeLista [] = 0
sumaDeLista (x:xs) = x + sumaDeLista xs

-- 2) Formulas Válidas [3 puntos]

-- problema formulasValidas (formulas: seq⟨String x String⟩) : Bool {
--   requiere: {True}
--   asegura: {(res = true) <=> formulas no contiene nombres repetidos, es decir que cada candidato está en una única fórmula (no se puede ser candidato a presidente y a vicepresidente ni en la misma fórmula ni en fórmulas distintas)}
-- }

formulasValidas :: [(String,String)] -> Bool
formulasValidas [] = True
formulasValidas [x] = not(estaEnMismaFormula x)
formulasValidas ((x,y):xs) 
    | estaRepeLista x xs = False
    | estaRepeLista y xs = False
    |otherwise = formulasValidas xs

estaEnMismaFormula :: (String,String) -> Bool
estaEnMismaFormula (x,y) = x == y

estaRepetidoEnLaLista :: (String,String) -> [(String,String)] -> Bool
estaRepetidoEnLaLista (x,y) [] = estaEnMismaFormula (x,y)
estaRepetidoEnLaLista x (u:v:xs) 
    | x == v || tuplaInversa x == v = False
    |otherwise = estaRepetidoEnLaLista x xs

tuplaInversa :: (String,String) -> (String,String)
tuplaInversa (x,y) = (y,x)

estaRepeLista :: String -> [(String,String)] -> Bool
estaRepeLista x [] = False
estaRepeLista x ((u,v):us) 
    | x==u = True
    | x == v = True
    |otherwise = estaRepeLista x us

-- 3) Porcentaje de Votos [3 puntos]

-- problema porcentajeDeVotos (presidente: String, formulas: seq⟨String x String⟩,votos:seq< Z >) : R {
--   requiere: {La primera componente de algún elemento de formulas es presidente}
--   requiere: {formulasValidas(formulas)}
--   requiere: {|formulas| = |votos|}
--   requiere: {Todos los elementos de votos son mayores o iguales a 0}
--   requiere: {Hay al menos un elemento de votos que es mayor que estricto que 0}
--   asegura: {res es el porcentaje de votos que obtuvo la fórmula encabezada por presidente sobre el total de votos afirmativos}
-- }
-- Para resolver este ejercicio pueden utilizar la siguiente función que devuelve como Float la división entre dos números de tipo Int:

-- division :: Int -> Int -> Float
-- division a b = (fromIntegral a) / (fromIntegral b) 

porcentajeDeVotos :: String -> [(String,String)] -> [Int] -> Float
porcentajeDeVotos n xs ys = (fromIntegral(votosPresidente)/fromIntegral(votosTotales))*100
  where votosPresidente = dameValorEnIndiceN (devolverIndiceDeFormula n xs) ys
        votosTotales = sumaDeLista ys
-- devolverIndiceDeFormulaAux :: String -> [(String,String)] -> (String,String)
-- devolverIndiceDeFormulaAux n 

devolverIndiceDeFormula :: String -> [(String,String)] -> Int
devolverIndiceDeFormula _ [] = -99
devolverIndiceDeFormula n ((x,_):xs)
  |n == x = 0
  |otherwise = (devolverIndiceDeFormula n xs) + 1

dameValorEnIndiceN :: (Eq t) => Int -> [t] -> t
dameValorEnIndiceN i xs = dameValorEnIndiceNAux i 0 xs

dameValorEnIndiceNAux :: Int -> Int -> [t] -> t
dameValorEnIndiceNAux i v (x:xs) 
  | i == v = x
  |otherwise = dameValorEnIndiceNAux i (v+1) xs


-- 4) Próximo Presidente [3 puntos]

-- problema proximoPresidente (formulas: seq⟨String x String⟩, votos:seq< Z >) : String {
--   requiere: {formulasValidas(formulas)}
--   requiere: {|formulas| = |votos|}
--   requiere: {Todos los elementos de votos son mayores o iguales a 0}
--   requiere: {Hay al menos un elemento de votos mayores estricto a 0}
--   requiere: {|formulas| > 0}
--   asegura: {res es el candidato a presidente de formulas más votado de acuerdo a los votos contabilizados en votos}
-- }

-- -}

proximoPresidente :: [(String,String)] -> [Int] -> String
proximoPresidente xs ys = fst (dameValorEnIndiceN (devolverIndiceDeMasVotos (maxVoto ys) ys) xs)

devolverIndiceDeMasVotos :: Int -> [Int] -> Int
devolverIndiceDeMasVotos _ [] = -99
devolverIndiceDeMasVotos n (x:xs)   
  | n == x =0
  |otherwise = (devolverIndiceDeMasVotos n xs) + 1

maxVoto :: [Int] -> Int
maxVoto [] = -1
maxVoto [x] = x
maxVoto (x:y:xs) 
  | x >= y = maxVoto(x:xs)
  |otherwise = maxVoto(y:xs)
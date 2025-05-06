--  Ejercicio 1 (2 puntos)

-- problema hayQueCodificar (c: Char, mapeo: seq⟨Char x Char⟩ ) : Bool {
--   requiere: {No hay elementos repetidos entre las primeras componentes de mapeo}
--   requiere: {No hay elementos repetidos entre las segundas componentes de mapeo}
--   asegura: {res = true <=> c es igual a la primera componente de alguna tupla de mapeo}
-- }

hayQueCodificar :: Char -> [(Char,Char)] -> Bool
hayQueCodificar _ [] = False
hayQueCodificar c ((x,y):xs) 
    | c == x = True
    |otherwise = hayQueCodificar c xs

--  Ejercicio 2 (2 puntos)

-- problema cuantasVecesHayQueCodificar (c: Char, frase: seq⟨Char⟩, mapeo: seq⟨Char x Char⟩ ) : Z {
--   requiere: {No hay elementos repetidos entre las primeras componentes de mapeo}
--   requiere: {No hay elementos repetidos entre las segundas componentes de mapeo}
--   requiere: {|frase| > 0 }
--   requiere: {c pertenece a frase}
--   asegura: {(res = 0 y hayQueCodificar (c, mapeo) = false) o (res = cantidad de veces que c aparece en frase y hayQueCodificar (c, mapeo) = true)}
-- }
type Frase = [Char]
type Mapeo = [(Char,Char)]
cuantasVecesHayQueCodificar :: Char -> [Char] -> [(Char,Char)] -> Int
cuantasVecesHayQueCodificar c f m 
    |hayQueCodificar c m == False = 0
    |otherwise = cantidadDeRepeticiones c f

cantidadDeRepeticiones :: Char -> Frase -> Int
cantidadDeRepeticiones _ [] = 0
cantidadDeRepeticiones c (x:xs)     
    | c == x = 1 + cantidadDeRepeticiones c xs
    |otherwise = cantidadDeRepeticiones c xs



--      Ejercicio 3 (2 puntos)

-- problema laQueMasHayQueCodificar (frase: seq⟨Char⟩, mapeo: seq⟨Char x Char⟩ ) : Char {
--   requiere: {No hay elementos repetidos entre las primeras componentes de mapeo}
--   requiere: {No hay elementos repetidos entre las segundas componentes de mapeo}
--   requiere: {|frase| > 0 }
--   requiere: {Existe al menos un c que pertenece a frase y hayQueCodificar(c, mapeo)=true}
--   asegura: {res = c donde c es el caracter tal que cuantasVecesHayQueCodificar(c, frase, mapeo) es mayor a cualquier otro caracter perteneciente a frase}
--   asegura: {Si existen más de un caracter c que cumple la condición anterior, devuelve el que aparece primero en frase }

laQueMasHayQueCodificar :: [Char] -> [(Char,Char)] -> Char
laQueMasHayQueCodificar [x] _ = x
laQueMasHayQueCodificar (x:xs) m =  compararLetras (x:xs) (x:xs) m

compararLetras :: [Char]-> [Char]-> [(Char,Char)] -> Char
compararLetras [x] f _ = x
compararLetras  (c:v:cs) f m
    | cuantasVecesHayQueCodificar c f m >= cuantasVecesHayQueCodificar v f m = compararLetras (c:cs) f m
    |otherwise = compararLetras (v:cs) f m

--[('a', 'b'), ('b', 'c'),('c', 'd'), ('d', 'e'), ('e', 'f'),  ('f', 'g'), ('g', 'h'), ('h', 'i'), ('i', 'j'), ('j', 'k'),  ('k', 'l'), ('l', 'm'), ('m', 'n'), ('n', 'o'), ('o', 'p'), ('p', 'q'), ('q', 'r'), ('r', 's'), ('s', 't'), ('t', 'u'), ('u', 'v'), ('v', 'w'), ('w', 'x'), ('x', 'y'), ('y', 'z')] 

--  Ejercicio 4 (3 puntos)

-- problema codificarFrase (frase: seq⟨Char⟩, mapeo: seq⟨Char x Char⟩ ) : seq ⟨Char⟩ {
--   requiere: {No hay elementos repetidos entre las primeras componentes de mapeo}
--   requiere: {No hay elementos repetidos entre las segundas componentes de mapeo}
--   requiere: {|frase| > 0 }
--   asegura: {|res| = | frase|}
--   asegura: { Para todo 0 <= i < |frase| si hayQueCodificar(frase[i], mapeo) = true entonces res[i]= (mapeo[j])1, para un j tal que 0 <= j < |mapeo| y mapeo[j])0=frase[i]}
--   asegura: { Para todo 0 <= i < |frase| si hayQueCodificar(frase[i], mapeo) = false entonces res[i]= frase[i]}
-- } 


codificarFrase :: [Char] -> [(Char,Char)] -> [Char]
codificarFrase [] _ = []
codificarFrase(x:xs) m 
    | esValidoParaTodaLetra (x:xs) m = intercambiar x m : codificarFrase (eliminarBlancos xs) m 
    | otherwise = (x:xs)


esValidoParaTodaLetra :: Frase -> Mapeo -> Bool
esValidoParaTodaLetra [x] m = hayQueCodificar x m
esValidoParaTodaLetra (x:xs) m 
    | hayQueCodificar x m = esValidoParaTodaLetra (eliminarBlancos xs) m
    | otherwise = False

intercambiar :: Char -> Mapeo -> Char
intercambiar c m 
    | hayQueCodificar c m = devolverCifrado c m
    |otherwise = c

eliminarBlancos :: Frase -> Frase
eliminarBlancos [] = []
eliminarBlancos (x:xs) 
    | x == ' ' = eliminarBlancos xs
    |otherwise = x:eliminarBlancos xs

devolverCifrado :: Char -> Mapeo -> Char
devolverCifrado c [] = c
devolverCifrado c ((x,y):xs) 
    |c == x = y
    |otherwise = devolverCifrado c xs
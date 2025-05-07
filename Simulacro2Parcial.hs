

-- ENUNCIADO
 
--     Ejercicio 1
--     Para empezar a diseñar la novedosa y rupturista red social Y el famoso Elio Mark nos ha pedido que desarrollemos algunas funciones básicas, 
--que tendrán como objetido representar algunas relaciones e interacciones entre los usuarios.
-- Para esto nos envió las siguientes especificaciones en lenguaje semiformal y nos pidió que hagamos el desarrollo enteramente en Haskell,
--   utilizando los tipos requeridos y solamente las funciones que se ven en Introducción a la Programación de Exactas-UBA.
 
--     problema relacionesValidas (relaciones: seq⟨String x String⟩) : Bool {
--       requiere: {True}
--       asegura: {(res = true) <=> relaciones no contiene ni tuplas repetidas1, ni tuplas con ambas componentes iguales}
--     }
--     1 A los fines de este problema consideraremos que dos tuplas son iguales si el par de elementos que las componen (sin importar el orden) son iguales.
 
--     problema personas (relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
--       requiere: {relacionesValidas(relaciones)}
--       asegura: {res no tiene elementos repetidos}
--       asegura: {res tiene exactamente los elementos que figuran en alguna tupla de relaciones, en cualquiera de sus posiciones}
--     }
 
--     problema amigosDe (persona: String, relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
--       requiere: {relacionesValidas(relaciones)}
--       asegura: {res tiene exactamente los elementos que figuran en las tuplas de relaciones en las que una de sus componentes es persona}
--     }
 
--     problema personaConMasAmigos (relaciones: seq⟨String x String⟩) : String {
--       requiere: {relaciones no vacía}
--       requiere: {relacionesValidas(relaciones)}
--       asegura: {res es el Strings que aparece más veces en las tuplas de relaciones (o alguno de ellos si hay empate)}
--     }

-- relacionesValidas :: [(String,String)]->Bool
-- relacionesValidas (x:y:xs) = relacionesValidasAux

relacionesValidas :: [(String,String)]->Bool
relacionesValidas [] = True
relacionesValidas [x] = tuplaValida x
relacionesValidas (x:y:xs) 
    |compararTuplas x y = relacionesValidas(x:xs)
    |otherwise = False

tuplaValida :: (String,String) -> Bool
tuplaValida (x,y) = x /= y 

compararTuplas :: (String,String) -> (String,String) -> Bool
compararTuplas (x,y) (u,v) 
    | tuplaValida (x,y) && tuplaValida (u,v) = (x==u && y/=v) || (x/=u && y==v) || (x/=v && y==u) || (x == v && y/=u)
    |otherwise = False

----------------------------------------------------------

--Ejercicio 2
personas :: [(String,String)] -> [String]
personas xs = eliminarRepetidos (conectarTuplas xs)

estaRepetido :: (String,String) ->(String,String) -> String
estaRepetido (x,y) (u,v) 
    |estaEn x (u,v)  = x
    | estaEn y (u,v) = y

estaEn :: String -> (String,String) -> Bool
estaEn s (x,y) = s == x || s == y

conectarTuplas :: [(String,String)] -> [String]
conectarTuplas [(x,y)] = x:[y]
conectarTuplas ((x,y):xs) = x:y:conectarTuplas xs

eliminarRepetidos :: [String] -> [String]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs) = x:eliminarRepetidos( eliminarNombreRepetido x xs)

eliminarNombreRepetido :: String -> [String] -> [String]
eliminarNombreRepetido _ [] = []
eliminarNombreRepetido s (x:xs)
    |s == x = eliminarNombreRepetido s xs
    |otherwise = x : eliminarNombreRepetido s (xs)

---------------------------------------------------------------------------------------
--EJERCICIO 3
--     problema amigosDe (persona: String, relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
--       requiere: {relacionesValidas(relaciones)}
--       asegura: {res tiene exactamente los elementos que figuran en las tuplas de relaciones en las que una de sus componentes es persona}
--     }

amigosDe :: String -> [(String,String)]->[String]
amigosDe n [] = []
amigosDe n ((x,y):xs) 
    | estaEn n (x,y) = y:amigosDe n xs
    |otherwise = amigosDe n xs
-----------------------------------------------------------------------------------------
--EJERCICIO 4
 --problema personaConMasAmigos (relaciones: seq⟨String x String⟩) : String {
--       requiere: {relaciones no vacía}
--       requiere: {relacionesValidas(relaciones)}
--       asegura: {res es el Strings que aparece más veces en las tuplas de relaciones (o alguno de ellos si hay empate)}
--     }

personaConMasAmigos :: [(String,String)] -> String
personaConMasAmigos [(x,_)] = x
personaConMasAmigos xs = nombreMasRepetido (conectarTuplas xs) xs

numeroDeRelaciones :: String -> [(String,String)] -> Int
numeroDeRelaciones n m = longitud (amigosDe n m)

longitud :: [String] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud (xs)

nombreMasRepetido :: [String] ->[(String,String)]-> String
nombreMasRepetido [x] _ = x
nombreMasRepetido (xs) m = nombreMasRepetidoAux xs xs m

nombreMasRepetidoAux :: [String] -> [String] -> [(String,String)]  -> String
nombreMasRepetidoAux [x] _ _ = x
nombreMasRepetidoAux (x:y:xs) f m 
    |numeroDeRelaciones x  m >= numeroDeRelaciones y  m = nombreMasRepetidoAux (x:xs) f m
    |otherwise =nombreMasRepetidoAux (y:xs) f m
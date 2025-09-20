module Practica01 where

--FUNCIONES
--Si x es menor a 0 (es decir, es negativo), entonces se multiplica por -1. Si no, se deja igual
valorAbs :: Int -> Int
valorAbs x = if x < 0 then -x else x

-- Si x es divisor de y, y/x deberia dar una division "limpia", entonces si se vuelve a multiplicar por el divisor deberia dar el mismo numero que el original
esDivisor :: Int -> Int -> Bool
esDivisor x y = let z = y `div` x
                in z * x == y

-- Solo es calcular ax^2 + bx + c
cuadratica :: Float -> Float -> Float -> Float -> Float
cuadratica x y z v = (x * v * v) + (y * v) + z

-- Si los denominadores son iguales, solo se suman los numeradores. Si no, a/b + c/d = [(a*d) + (c*b)]/(b*d)
sumaFracciones :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumaFracciones (a,b) (c,d) = if b == d then
                             (a + c, b)
                             else
                             ( (a*d) + (c*b) , (b*d) )

comparador :: Float -> Float -> Int
comparador n m = if n == m then 0
                 else if n > m then 1
                 else -1

puntoMedio :: (Float, Float) -> (Float, Float) -> (Float, Float)
puntoMedio (a, b) (c, d)= ((a + c)/2, (b + d)/2)

--RELACIONES

--Sean A y B conjuntos tales que A = B = {1,2,3,...,30}
--1. relacionDivisor :: Rel Int Int 
--En esta relacion R1, tenemos que aR1b si a y b tienen la misma paridad y a es divisor de b
--La funcion relacionDivisor admite dos numeros enteros a y b si cumplen las 2 condiciones, devuelve True en caso contrario False
--Primero definimos el tipo Rel para las relaciones entre dos conjuntos
type Rel a b = a -> b -> (a, b)
--La funcion devuelve la R1 sabiendo que aR1b si a y b tienen la misma paridad y a es divisor de b 
relacionDivisor :: Rel Int Int
relacionDivisor a b
  | even a == even b && b `mod` a == 0 = (a, b)
--funcion auxiliar even la utilice para verificar si los numeros son pares
--2. relacionSumaEspecial :: Rel Int Int
--La funcion devuelve la R2 sabiendo que aR2b si a + b es multiplo de 5 y a < b
relacionSumaEspecial :: Rel Int Int
relacionSumaEspecial a b
  | (a + b) `mod` 5 == 0 && a < b = (a, b)
--funcion auxiliar even la utilice para verificar si los numeros son pares
--3. relacionCongruentesModulo n :: Int -> Rel Int Int
--La funcion devuelve la R3 sabiendo que aR3b  se debe recibir un entero n y tenemos que aR3b si a %n = b %n con a̸ = b
relacionCongruentesModuloN :: Int -> Rel Int Int
relacionCongruentesModuloN n a b
  | a `mod` n == b `mod` n && a /= b = (a, b)

--NATURALES
-- Cero es natural, Suc Cero es natural, Suc Suc Cero es natural, etc.
data Natural = Cero | Suc Natural deriving (Show,Eq) --Esto es para que se muestre y que se puedan comparar

--Si el numero tiene al menos dos sucesores va pasar la primera regla, aun si n es natural con varios o un sucesor, y si el numero tiene solo un sucesor devuelve falso, tomamos cero como par para poder terminar la funcion al evaluar n.  
esPar :: Natural -> Bool
esPar Cero = True
esPar (Suc (Suc n)) = esPar n
esPar (Suc n) = False

iguales :: Natural -> Natural -> Bool
iguales Cero Cero = True
iguales Cero (Suc y) = False
iguales (Suc x) Cero = False
iguales (Suc x) (Suc y) = iguales x y

maximo :: Natural -> Natural -> Natural 
maximo Cero Cero = Cero
maximo Cero (Suc y) = Suc y
maximo (Suc x) Cero = Suc x
maximo (Suc x) (Suc y) = Suc (maximo x y)

multiplicacion :: Natural -> Natural -> Natural
multiplicacion Cero m = Cero
multiplicacion (Suc Cero) m = m
multiplicacion (Suc n) m = suma m (multiplicacion n m) --necesito la suma
--multiplicacion (Suc n) m = suma n (multiplicacion n m) MAL HECHO

suma :: Natural -> Natural -> Natural
suma Cero m = m
suma (Suc n) m = Suc (suma n m)

potencia :: Natural -> Natural -> Natural
potencia n Cero = Suc(Cero)
potencia n (Suc m) = multiplicacion n (potencia n m)

import Data.Char (digitToInt)

-- Función para obtener los divisores de un número
divisores :: Int -> [Int]
divisores n = [x | x <- [1..(n `div` 2)], n `mod` x == 0]

-- Función para sumar los divisores de un número
sumaDivisores :: Int -> Int
sumaDivisores n = sum (divisores n)

-- Función para determinar el tipo de número basado en la suma de sus divisores
tipoNumero :: Int -> String
tipoNumero n
  | sumaDivisores n == n = "Engineering"
  | sumaDivisores n > n = "Administratives"
  | otherwise = "Humanities"

-- Función para calcular el tipo de número basado en la suma alícuota de los dígitos 4 y 5
calcularTipo :: Int -> String
calcularTipo numero =
    let strNumero = show numero
        digito4 = digitToInt (strNumero !! 3)
        digito5 = digitToInt (strNumero !! 4)
        numDigitos = digito4 * 10 + digito5
        sumaAlcuota = sumaDivisores numDigitos
    in tipoNumero sumaAlcuota

-- Función para determinar el semestre basado en los primeros tres dígitos
semestreIn :: Int -> String
semestreIn n = 
  let lista = show n
      tomaDos = take 2 lista
      tomaTres = lista !! 2
  in "20" ++ tomaDos ++ "-" ++ [tomaTres]

-- Función para determinar si un número es par o impar
paridad :: Int -> String
paridad n
  | n `mod` 2 == 0 = "even"
  | otherwise      = "odd"

-- Función principal del programa
main :: IO ()
main = do
    numero <- readLn :: IO Int
    let tipo = calcularTipo numero
        semestre = semestreIn numero
        numX = read (drop 5 (show numero)) :: Int
        paridadNumX = paridad numX
    putStrLn $ semestre ++ " " ++ tipo ++ " num" ++ show numX ++ " " ++ paridadNumX
    
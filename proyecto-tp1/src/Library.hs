module Library where
import PdePreludat


doble :: Number -> Number
doble numero = numero + numero

sueldoBase :: String -> Number
sueldoBase base
    |base == "titular" = 149000
    |base == "adjunto" = 116000
    |base == "ayudante" = 60000

incrementoPorcentual :: Number -> Number
incrementoPorcentual anios
    |anios >= 3 && anios < 5 = 1.2
    |anios >= 5 && anios < 10 = 1.3
    |anios >= 10 && anios < 24 = 1.5
    |anios >= 24 = 2.2

importe :: Number -> Number
importe horas
    |horas > 5 && horas <= 15 = 1
    |horas > 15 && horas <= 25 = 2
    |horas > 25 && horas <= 35 = 3
    |horas > 35 && horas <= 45 = 4
    |horas > 45 && horas <= 50 = 5


calcularSueldo :: String -> Number -> Number -> Number
calcularSueldo cargo antiguedad cantHoras =
    sueldoBase cargo * incrementoPorcentual antiguedad * importe cantHoras

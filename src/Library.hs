module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras| Papas
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas =  10


data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

cuartoDeLibra = Hamburguesa {precioBase = 20, ingredientes = [Pan,Carne,Cheddar,Pan]}

pdepBurger = precioDeLaPdep .agregarIngrediente Panceta . agregarIngrediente Cheddar . agrandarHamburguesa . agrandarHamburguesa $ cuartoDeLibra

dobleCuarto = precioDeHamburguesa. agregarIngrediente Cheddar . agrandarHamburguesa $ cuartoDeLibra

bigPdep = precioDeHamburguesa. agregarIngrediente Papas $ dobleCuarto

delDia :: Hamburguesa -> Hamburguesa
delDia = agrandarHamburguesa. agrandarHamburguesa

esDeCarne :: [Ingrediente] -> Bool
esDeCarne = foldr (\ ingredientes -> (||) (ingredientes == Carne)) False

agrandarHamburguesa :: Hamburguesa -> Hamburguesa
agrandarHamburguesa unaHamburguesa
    | Carne `elem` ingredientes unaHamburguesa = agregarIngrediente Carne unaHamburguesa
    | Pollo`elem` ingredientes unaHamburguesa = agregarIngrediente Pollo unaHamburguesa
    | otherwise = unaHamburguesa

agregarIngrediente :: Ingrediente->Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente unaHamburguesa = unaHamburguesa { precioBase= precioBase unaHamburguesa + precioIngrediente ingrediente,ingredientes = agregarALaHAmburguesa ingrediente (ingredientes unaHamburguesa) }

agregarALaHAmburguesa :: Ingrediente -> [Ingrediente]-> [Ingrediente]
agregarALaHAmburguesa ingrediente [] = []
agregarALaHAmburguesa ingrediente (x:xs) 
    |ingrediente == Papas = ingrediente : x : xs  
    |otherwise= x :ingrediente:xs --se supone que la primera es un pan y el resto es la hamburguesa

descuento :: Number-> Hamburguesa -> Hamburguesa
descuento porcentaje unaHamburguesa = unaHamburguesa { precioBase = precioBase unaHamburguesa - precioBase unaHamburguesa * porcentaje/10 }

precioDeHamburguesa :: Hamburguesa ->Hamburguesa
precioDeHamburguesa unaHamburguesa = unaHamburguesa {precioBase = sum.map precioIngrediente $ ingredientes unaHamburguesa, ingredientes=ingredientes unaHamburguesa}

precioDeLaPdep :: Hamburguesa ->Hamburguesa
precioDeLaPdep unaHamburguesa = unaHamburguesa {precioBase=110, ingredientes=ingredientes unaHamburguesa}




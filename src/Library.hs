{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Library where
import PdePreludat


data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras| Papas | PatyVegano | BaconTofu | PanIntegral
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas =  10
precioIngrediente PatyVegano = 10
precioIngrediente PanIntegral = 3


data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

cuartoDeLibra = Hamburguesa 20 [Pan,Carne,Cheddar,Pan]

agrandarHamburguesa :: Hamburguesa -> Hamburguesa
agrandarHamburguesa unaHamburguesa
    | Carne `elem` ingredientes unaHamburguesa = agregarIngrediente Carne unaHamburguesa
    | Pollo`elem` ingredientes unaHamburguesa = agregarIngrediente Pollo unaHamburguesa
    | PatyVegano `elem` ingredientes unaHamburguesa = agregarIngrediente PatyVegano unaHamburguesa
    | otherwise = unaHamburguesa

agregarIngrediente :: Ingrediente->Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente unaHamburguesa =
     unaHamburguesa {ingredientes = agregarALaHAmburguesa ingrediente (ingredientes unaHamburguesa)}

agregarALaHAmburguesa :: Ingrediente -> [Ingrediente]-> [Ingrediente]
agregarALaHAmburguesa ingrediente [] = []
agregarALaHAmburguesa ingrediente (x:xs)
    |ingrediente == Papas = ingrediente : x : xs
    |otherwise= x :ingrediente:xs --se supone que la primera es un pan y el resto es la hamburguesa

descuento :: Number-> Hamburguesa -> Hamburguesa
descuento porcentaje unaHamburguesa = 
    unaHamburguesa { precioBase = precioBase unaHamburguesa - precioBase cuartoDeLibra * porcentaje/100 }

precioDeLaHamburguesa :: Hamburguesa -> Hamburguesa
precioDeLaHamburguesa  unaHamburguesa = 
    unaHamburguesa {precioBase =(+) (precioBase cuartoDeLibra). sum . map precioIngrediente  $ ingredientes unaHamburguesa}

pdepBurger =
    descuento descuentoDePdep.
    precioDeLaHamburguesa.
    agregarIngrediente Panceta .
    agregarIngrediente Cheddar .
    agrandarHamburguesa .
    agrandarHamburguesa
    $ cuartoDeLibra

descuentoDePdep :: Number
descuentoDePdep = 20

dobleCuarto =
    precioDeLaHamburguesa.
    agregarIngrediente Cheddar .
    agrandarHamburguesa
    $ cuartoDeLibra

bigPdep =
    precioDeLaHamburguesa.
    agregarIngrediente Curry
    $ dobleCuarto

descuentoDelDia :: Number
descuentoDelDia = 30

delDia :: Hamburguesa -> Hamburguesa
delDia = descuento descuentoDelDia. precioDeLaHamburguesa .agregarIngrediente Papas  

-- =========================Parte 3====================================

hacerVeggie :: Hamburguesa -> Hamburguesa 
hacerVeggie unaHamburguesa = unaHamburguesa {ingredientes = map ingredientesAVergano (ingredientes unaHamburguesa)}

ingredientesAVergano :: Ingrediente -> Ingrediente
ingredientesAVergano Carne = PatyVegano
ingredientesAVergano Cheddar = QuesoDeAlmendras
ingredientesAVergano Pollo = PatyVegano
ingredientesAVergano Panceta = BaconTofu
ingredientesAVergano otro = otro  

cambiarPan :: Ingrediente -> Ingrediente 
cambiarPan Pan = PanIntegral
cambiarPan otro = otro

cambiarPanDePati :: Hamburguesa -> Hamburguesa 
cambiarPanDePati unaHamburguesa = unaHamburguesa {ingredientes = map cambiarPan (ingredientes unaHamburguesa)}


dobleCuartoVegano :: Hamburguesa 
dobleCuartoVegano = cambiarPanDePati. hacerVeggie $ dobleCuarto
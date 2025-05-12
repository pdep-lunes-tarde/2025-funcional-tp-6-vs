module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

mcPollo = Hamburguesa 10 [Pan,Pollo,Pan]
veggieBurguer = Hamburguesa 8 [Pan,PatyVegano,Pan]

hamburguesaTriste = Hamburguesa 2 [Pan,Cheddar,Pan]

baconBurguer = Hamburguesa 12 [Pan,Carne,Panceta,Pan]

correrTests :: IO ()
correrTests = hspec $ do
    describe "agrandar" $ do
        it "Si se agranda una hamburguesa que tiene Carne, se le agrega otra carne " $ do
            agrandarHamburguesa cuartoDeLibra `shouldBe` Hamburguesa 20 [Pan,Carne,Carne,Cheddar,Pan] 
        it "Si se agranda una hamburguesa que tiene Pollo, se le agrega otro Pollo" $ do
            agrandarHamburguesa mcPollo `shouldBe` Hamburguesa 10 [Pan,Pollo,Pollo,Pan] 
        it "Si se agranda una hamburguesa que tiene Paty Vegano, se le agrega otro Paty " $ do
            agrandarHamburguesa veggieBurguer `shouldBe` Hamburguesa 8 [Pan,PatyVegano,PatyVegano,Pan] 
        it "Si se agranda una hamburguesa que no tiene ningun ingrediente base, queda igual" $ do
            agrandarHamburguesa hamburguesaTriste `shouldBe` Hamburguesa 2 [Pan,Cheddar,Pan] 

    describe "agregarIngrediente" $ do
        it "se le agrega un ingrediente elegido a la hamburguesa" $ do
            agregarIngrediente Pollo hamburguesaTriste  `shouldBe` Hamburguesa 2 [Pan,Pollo,Cheddar,Pan]

    describe "Tipos de Hamburguesas" $ do
        it "Un doble Cuarto que se agranda dos veces y se le agrega panceta y chedar, su precio es de 110 " $ do
            pdepBurger `shouldBe` Hamburguesa 110 [Pan,Panceta,Cheddar,Carne,Carne,Carne,Cheddar,Pan] 
        it "Un cuarto de libra que se agranda con extra chedar, su precio es de 84 " $ do
            dobleCuarto `shouldBe` Hamburguesa 84 [Pan,Cheddar,Carne,Carne,Cheddar,Pan]
        it "Una hamburguesa doble cuarto que se le agrega curry, su precio es de 89 " $ do
            bigPdep `shouldBe` Hamburguesa 89 [Pan,Curry,Cheddar,Carne,Carne,Cheddar,Pan]
        it "Promo del dia, le agrega papas a la hamburguesa y tiene un descuento de 30%" $ do
            delDia dobleCuarto `shouldBe` Hamburguesa 88 [Papas, Pan,Cheddar,Carne,Carne,Cheddar,Pan]

    describe "hacerVeggie" $ do
        it "Hacer una hamburguesa veggie que tiene carne o pollo, lo cambia a Paty vegano" $ do
            hacerVeggie mcPollo `shouldBe` Hamburguesa 10 [Pan,PatyVegano,Pan]
        it "Hacer una hamburguesa veggie que tiene cheddar, lo cambia a Queso de almendras" $ do
            hacerVeggie hamburguesaTriste `shouldBe` Hamburguesa 2 [Pan,QuesoDeAlmendras,Pan]
        it "Hacer una hamburguesa veggie que tiene panceta, la cambia a bacon de tofu" $ do
            hacerVeggie baconBurguer `shouldBe` Hamburguesa 12 [Pan,PatyVegano,BaconTofu,Pan]

    describe "cambiarPanDePati" $ do
        it "Cambia el pan de una hamburguesa por pan integral" $ do
            cambiarPanDePati mcPollo `shouldBe` Hamburguesa 10 [PanIntegral,Pollo,PanIntegral]
    describe "dobleCuartoVegano" $ do
        it "Una hamburguesa doble cuarto de libra la hacemos vegana con pan integral y precio de 89 " $ do
            dobleCuartoVegano `shouldBe` Hamburguesa 84 [PanIntegral, QuesoDeAlmendras, PatyVegano, PatyVegano, QuesoDeAlmendras, PanIntegral] 
       

-- precioIngrediente Carne = 20
-- precioIngrediente Pan = 2
-- precioIngrediente Panceta = 10
-- precioIngrediente Cheddar = 10
-- precioIngrediente Pollo =  10
-- precioIngrediente Curry = 5
-- precioIngrediente QuesoDeAlmendras = 1
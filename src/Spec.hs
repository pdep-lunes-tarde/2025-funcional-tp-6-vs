module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "Perte 1 y 2" $ do
        it "Una hamburgusa a la que se le agranda dos veces y se le agrega panceta y chedar y su precio es de 110 " $ do
            pdepBurger `shouldBe` Hamburguesa 110 [Pan,Panceta,Cheddar,Carne,Carne,Carne,Cheddar,Pan] 
        it "Una hamburguesa caurto de libre se la agrandada con extra chedar y un precio de 84 " $ do
            dobleCuarto `shouldBe` Hamburguesa 84 [Pan,Cheddar,Carne,Carne,Cheddar,Pan]
        it "Una hamburguesa doble cuarto que se le agrega curry y un precio de 89 " $ do
            bigPdep `shouldBe` Hamburguesa 89 [Pan,Curry,Cheddar,Carne,Carne,Cheddar,Pan]
        it "Una hamburguesa que agrandade con extra chedar y un precio de 84 " $ do
            delDia dobleCuarto `shouldBe` Hamburguesa 88 [Papas, Pan,Cheddar,Carne,Carne,Cheddar,Pan]

    describe "Perte 3" $ do
        it "Una hamburguesa doble cuarto de libra la hacemos vegana con pan integral y precio de 89 " $ do
            dobleCuartoVegano `shouldBe` Hamburguesa 110 [PanIntegral, QuesoDeAlmendras, PatyVegano, PatyVegano, QuesoDeAlmendras, PanIntegral] 
       

-- precioIngrediente Carne = 20
-- precioIngrediente Pan = 2
-- precioIngrediente Panceta = 10
-- precioIngrediente Cheddar = 10
-- precioIngrediente Pollo =  10
-- precioIngrediente Curry = 5
-- precioIngrediente QuesoDeAlmendras = 1
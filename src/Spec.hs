module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "La PdepBurguer" $ do
        it "Una hamburgusa a la que se le agranda dos veces y se le agrega panceta y chedar y su precio es de 110 " $ do
            pdepBurger `shouldBe` Hamburguesa 110 [Pan,Panceta,Cheddar,Carne,Carne,Carne,Cheddar,Pan] 
    describe "La PdepBurguer" $ do
        it "Una hamburguesa que agrandade con extra chedar y un precio de 84 " $ do
            dobleCuarto `shouldBe` Hamburguesa 84 [Pan,Cheddar,Carne,Carne,Cheddar,Pan] 

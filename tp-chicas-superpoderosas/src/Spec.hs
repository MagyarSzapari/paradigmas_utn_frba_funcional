module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do

  describe "1er Entrega - Test Punto 1" $ do
      it "danioPotencialDe mojoJojo es 64" $ do
        danioPotencialDe mojoJojo `shouldBe` 64
    
      it "danioPotencialDe de princesa es 89" $ do
         danioPotencialDe princesa `shouldBe` 89
    
      it "danioPotencialDe de  bandaGangrena es 40" $ do
         danioPotencialDe bandaGangrena `shouldBe` 40

  describe "1er Entrega - Test Punto 2" $ do
      it "mojoJojo puede atacar Saltadilla" $ do
        saltadilla `shouldSatisfy` puedeAtacarCiudad mojoJojo 
    
      it "princesa puede atacar Saltadilla" $ do
        saltadilla `shouldSatisfy` puedeAtacarCiudad princesa
    
      it "bandaGangrena no puede atacar Saltadilla" $ do
        saltadilla `shouldNotSatisfy` puedeAtacarCiudad bandaGangrena

  describe "1er Entrega - Test Punto 3" $ do
      it "bombon puede vencer a mojoJojo" $ do
        bombon `shouldSatisfy` puedeVencerAmenaza mojoJojo
    
      it "burbuja no puede vencer a princesa" $ do
        burbuja `shouldNotSatisfy` puedeVencerAmenaza princesa 

  describe "1er Entrega - Test Punto 4" $ do
      it "princesa es una amenaza de nivel alto" $ do
        princesa `shouldSatisfy` esNivelAlto
    
      it "bandaGangrena no es una amenaza de nivel alto" $ do
        bandaGangrena `shouldNotSatisfy` esNivelAlto  









  describe "Yendo al Nutricionista - Test Punto 1" $ do
      it "bombon se queda sin resistencia luego de consumir la sustancia X" $ do
        nivelDeResistencia (beberSustanciaX bombon) `shouldBe` 0

      it "burbuja se queda sin resistencia luego de consumir la sustancia X" $ do
        nivelDeResistencia (beberSustanciaX bombon) `shouldBe` 0

      it "bellota se queda sin resistencia luego de consumir la sustancia X" $ do
        nivelDeResistencia (beberSustanciaX bombon) `shouldBe` 0
        
      it "burbuja toma una cerveza con silico y queda con 2 amigos" $ do
        amigos (beberCerveza ["silico"] burbuja) `shouldBe` ["senoritaBelo", "silico"]    

      it "burbuja toma una cerveza con la senoritaBelo y queda con un amigo" $ do
        amigos (beberCerveza ["senoritaBelo"] burbuja)  `shouldBe` (amigos burbuja)

  describe "Yendo al Nutricionista- Test Punto 2" $ do

      it "bellota se toma un fernet y tiene su nueva habilidad de Chef de Asados" $ do
        habilidades (beberFernet bellota) `shouldBe` ["velocidad","super fuerza","Chef de Asados"]

  describe "Yendo al Nutricionista - Test Punto 3" $ do
      it "bellota luego de tomar gatorei queda igual" $ do
        nivelDeResistencia (beberGatorei bellota) `shouldBe` (nivelDeResistencia bellota)
    
      it "bombon luego de tomar gatorei queda con 65 puntos de resistencia" $ do
        nivelDeResistencia (beberGatorei bombon) `shouldBe` 65
      
      it "bombon toma 3 shots de vodka y pasa a llamarse bom" $ do
        nombre (beberVodka 3 bombon) `shouldBe` "bom"

  describe "Yendo al Nutricionista - Test Punto 4" $ do
      it "burbuja debe tener 20 de resistencia luego de consumir el caramelo líquido" $ do
        nivelDeResistencia (beberCarameloLiquido burbuja) `shouldBe` 20
    
      it "senoritaBelo debe tener 0 de resistencia luego de consumir caramelo líquido" $ do
        nivelDeResistencia (beberCarameloLiquido senoritaBelo) `shouldBe` 0

      it "senorCerdo le entra a la cocucha y queda igual" $ do
        habilidades (beberCocucha senorCerdo) `shouldBe` habilidades senorCerdo












  describe "Mi Villano Favorito - Tests" $ do
      it "mojoJojo ataca saltadilla, su poblacion final es 9" $ do
        cantidadHabitantes (atacarCiudad mojoJojo saltadilla)  `shouldBe` 9
      
      it "princesa ataca saltadilla, su poblacion final es 13" $ do
        cantidadHabitantes (atacarCiudad princesa saltadilla)  `shouldBe` 13

      it "bandaGangrena ataca saltadilla 1 vez, su poblacion final es 17 " $ do
        cantidadHabitantes (atacarCiudad bandaGangrena saltadilla)  `shouldBe` 17
      
      it "bandaGangrena ataca saltadilla 2 veces consecutivas, su poblacion final es 26" $ do
        cantidadHabitantes (atacarCiudad bandaGangrena (atacarCiudad bandaGangrena saltadilla)) `shouldBe` 26

      it "bandaGangrena ataca saltadilla 2 veces consecutivas, pasa a llamarse Gangrena City" $ do
        nombreCiudad (atacarCiudad bandaGangrena (atacarCiudad bandaGangrena saltadilla)) `shouldBe` "Gangrena City"












  describe "Darle Play" $ do
      it "Capitulo 1 afecta a Saltadilla" $ do
        cantidadHabitantes (darlePlay saltadilla cap1) `shouldBe` 9 

      it "Capitulo 1 NO afecta a Pueblo Paleta" $ do
        cantidadHabitantes (darlePlay puebloPaleta cap1) `shouldBe` 10

      it "Capitulo 2 afecta a Saltadilla" $ do
        cantidadHabitantes (darlePlay saltadilla cap2) `shouldBe` 13

      it "Capitulo 2 NO afecta a Stardew Valley" $ do
        cantidadHabitantes (darlePlay  stardewValley cap2) `shouldBe` 27

      it "Capitulo 3 afecta a Stardew Valley" $ do
        cantidadHabitantes (darlePlay stardewValley cap3) `shouldBe` 19

      it "Capitulo 3 NO afecta a Pueblo Paleta" $ do
        cantidadHabitantes (darlePlay puebloPaleta cap3) `shouldBe` 10

      it "Capitulo 4 afecta a Pueblo Paleta" $ do
        cantidadHabitantes (darlePlay puebloPaleta cap4) `shouldBe` 12

      it "Capitulo 4 NO afecta a Saltadilla" $ do
        cantidadHabitantes (darlePlay saltadilla cap4) `shouldBe` 21



  describe "Maraton" $ do

      it "La temporada deja a Saltadilla con 1 habitante" $ do 
        cantidadHabitantes(maraton saltadilla temporada) `shouldBe` 1

      it "La temporada deja a Pueblo Paleta con 12 habitantes" $ do
        cantidadHabitantes(maraton puebloPaleta temporada) `shouldBe` 12



  

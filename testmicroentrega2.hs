import Test.Hspec
import MicroEntrega2

divisionPorCero = divide . lod 1 . swap . lod 2 . str 2 0 . str 1 2
division12Por4 = divide . lod 1 . swap . lod 2 . str 2 4 . str 1 12

runTests = hspec $ do
    describe "Test Punto 2 - Tests de NOP" $ do
        --it "NOP incrementa el program counter" $ do
        --  ((programCounter . nop) xt8088) `shouldBe` 1
        
        it "NOP no cambia el acumulador" $ do
            ((acumuladorA . nop) xt8088) `shouldBe` 0
      
        --it "Programa con 3 NOP avanza 3 veces el program counter" $ do
        --    ((programCounter . nop . nop . nop) xt8088) `shouldBe` 3
                
    describe "Test Punto 3 - Tests de programa Suma" $ do
        it "LODV de 5 lo carga en acumulador A" $ do
            ((acumuladorA . lodv 5) xt8088) `shouldBe` 5
        
        it "SWAP cambia los valores de ambos acumuladores" $ do
            ((acumuladorA . swap) fp20) `shouldBe` 24
    
        it "SWAP cambia los valores de ambos acumuladores (acumulador A)" $ do
            ((acumuladorB . swap) fp20) `shouldBe` 7

        it "SWAP cambia los valores de ambos acumuladores (acumulador B)" $ do
            ((acumuladorA . swap) fp20) `shouldBe` 24

        it "Suma 10 + 22 da 32 en Acumulador A" $ do
            ((acumuladorA . add . lodv 22 . swap . lodv 10) at8086) `shouldBe` 32

        it "Suma 10 + 22 da 0 en Acumulador B" $ do
            ((acumuladorB . add . lodv 22 . swap . lodv 10) at8086) `shouldBe` 0

    describe "Test Punto 4 - Tests de programa División" $ do
        it "STR 2 5 para la memoria" $ do
            ((memoria . str 2 5) at8086) `shouldBe` [1, 5, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

        it "LOD 2 de una memoria vacía debe dar 0" $ do
            (acumuladorA $ lod 2 xt8088) `shouldBe` 0

        it "Division por cero da error" $ do
            (mensajeError $ divisionPorCero xt8088) `shouldBe` "DIVISION BY ZERO"
    
        it "Division de 12 por 4 se resuelve bien en Acumulador A" $ do
            (acumuladorA $ division12Por4 xt8088) `shouldBe` 3

        it "Division de 12 por 4 blanquea Acumulador B" $ do
            (acumuladorB $ division12Por4 xt8088) `shouldBe` 0

        it "Division de 12 por 4 no deja el mensaje de error porque funciona bien" $ do
            (mensajeError $ division12Por4 xt8088) `shouldBe` ""
        
    describe "Test Punto 2.2 - Tests de programas Suma" $ do
        it "ejecutar el programa de suma deja el acumulador A con el resultado de dicha suma" $ do
            ((acumuladorA . ejecutar . cargarPrograma suma10y22) xt8088) `shouldBe` 32

        it "ejecutar el programa de suma deja el acumulador B en 0" $ do
            ((acumuladorB . ejecutar . cargarPrograma suma10y22) xt8088) `shouldBe` 0

        it "ejecutar el programa de suma sube el program counter" $ do
            ((programCounter . ejecutar . cargarPrograma suma10y22) xt8088) `shouldBe` 4

    describe "Test Punto 2.2 - Tests de programas Division" $ do
        it "ejecutar el programa de division por cero deja el acumulador A con 2" $ do
            ((acumuladorA . ejecutar . cargarPrograma division2por0) xt8088) `shouldBe` 2

        it "ejecutar el programa de division por cero deja el acumulador B en 0" $ do
            ((acumuladorB . ejecutar . cargarPrograma division2por0) xt8088) `shouldBe` 0

        it "ejecutar el programa de division por cero sube el program counter" $ do
            ((programCounter . ejecutar . cargarPrograma division2por0) xt8088) `shouldBe` 6
        
        it "ejecutar el programa de division por cero deja un mensaje de error" $ do
            ((mensajeError . ejecutar . cargarPrograma division2por0) xt8088) `shouldBe` "DIVISION BY ZERO"

        it "ejecutar el programa de division por cero deja la memoria con valores" $ do
            ((take 2 . memoria . ejecutar . cargarPrograma division2por0) xt8088) `shouldBe` [2,0]

    describe "Test Punto 2.3 - Tests de IFNZ" $ do
        it "ejecutar IFNZ sobre un micro que tiene acumulador A distinto de cero ejecuta las instrucciones - efecto sobre acumulador A" $ do
            ((acumuladorA . ifNZ [lodv 3, swap]) fp20) `shouldBe` 24
                    
        it "ejecutar IFNZ sobre un micro que tiene acumulador A distinto de cero ejecuta las instrucciones - efecto sobre acumulador B" $ do
            ((acumuladorB . ifNZ [lodv 3, swap]) fp20) `shouldBe` 3

        it "ejecutar IFNZ sobre un micro que tiene acumulador A igual a cero ejecuta las instrucciones - sin efecto sobre acumulador A" $ do
            ((acumuladorA . ifNZ [lodv 3, swap]) xt8088) `shouldBe` 0
                    
        it "ejecutar IFNZ sobre un micro que tiene acumulador A igual a cero ejecuta las instrucciones - sin efecto sobre acumulador B" $ do
            ((acumuladorB . ifNZ [lodv 3, swap]) xt8088) `shouldBe` 0

    describe "Test Punto 2.4 - Depuración de un programa" $ do
        it "Depuración de un programa - Deben quedar solo las instrucciones que modifican el estado interno del microprocesador" $ do
            ((length . depurar xt8088) [swap, nop, lodv 133, lodv 0, str 1 3, str 2 0]) `shouldBe` 2
        
        it "Depuración de un programa - Chequeo primera instrucción" $ do
            ((acumuladorA . flip ($) xt8088 . head . depurar xt8088) [swap, nop, lodv 133, lodv 0, str 1 3, str 2 0]) `shouldBe` 133
                    
        it "Depuración de un programa - Chequeo segunda instrucción" $ do
            ((flip (!!) 0 . memoria . flip ($) xt8088 . flip (!!) 1 . depurar xt8088) [swap, nop, lodv 133, lodv 0, str 1 3, str 2 0]) `shouldBe` 3
            
    describe "Test Punto 2.5 - Orden de la memoria" $ do
        it "Micro con la memoria ordenada" $ do
            (memoriaOrdenada at8086) `shouldBe` True
                    
        it "Micro con la memoria desordenada" $ do
            (memoriaOrdenada microDesorden) `shouldBe` False
                
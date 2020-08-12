module MicroEntrega2 where
    import Text.Show.Functions

    data Microprocesador = Microprocesador{
        acumuladorA :: Int,
        acumuladorB :: Int,
        programCounter :: Int,
        memoria :: [Int],
        mensajeError :: String,
        programa :: [Microprocesador -> Microprocesador]
    } deriving Show

    xt8088 = Microprocesador{
        acumuladorA = 0,
        acumuladorB = 0,
        programCounter = 0,
        memoria = replicate 1024 0,
        mensajeError = [],
        programa = []
    }
    
    fp20 = Microprocesador{
        acumuladorA = 7,
        acumuladorB = 24,
        programCounter = 0,
        memoria = [],
        mensajeError = [],
        programa = []
    }

    at8086 = Microprocesador{
        acumuladorA = 0,
        acumuladorB = 0,
        programCounter = 0,
        memoria = [1..20],
        mensajeError = [],
        programa = []
    }

    microDesorden = Microprocesador{
        acumuladorA = 0,
        acumuladorB = 0,
        programCounter = 0,
        memoria = [2,5,1,0,6,9],
        mensajeError = [],
        programa = []
    }

    memoriaInfinita = Microprocesador{
        acumuladorA = 0,
        acumuladorB = 0,
        programCounter = 0,
        memoria = [0..],
        mensajeError = [],
        programa = []
    }
    
    suma10y22 = [lodv 10, swap, lodv 22, add]
    division2por0 = [str 1 2, str 2 0, lod 2, swap, lod 1, divide]

    nop microprocesador = microprocesador

    incrementarProgramCounter microprocesador = microprocesador{
        programCounter = programCounter microprocesador + 1
    }

    lodv val microprocesador = microprocesador{
        acumuladorA = val
    }

    swap microprocesador = microprocesador{
        acumuladorB = acumuladorA microprocesador,
        acumuladorA = acumuladorB microprocesador
    }

    add microprocesador = microprocesador{
        acumuladorA = acumuladorA microprocesador + acumuladorB microprocesador,
        acumuladorB = 0
    }
    
    divide microprocesador | (acumuladorB microprocesador) /= 0 = dividir microprocesador
                           | otherwise = actualizarError "DIVISION BY ZERO" microprocesador
    dividir microprocesador = microprocesador{
        acumuladorA = div (acumuladorA microprocesador) (acumuladorB microprocesador),
        acumuladorB = 0
    }

    actualizarError error microprocesador = microprocesador{
        mensajeError = error
    }

    str addr val microprocesador = microprocesador{
        memoria = (++) ((++) (take (addr-1) (memoria microprocesador)) [val]) ((reverse.(take ((length (memoria microprocesador))-addr)).reverse) (memoria microprocesador))
    }

    lod addr microprocesador = microprocesador{
        acumuladorA = (memoria microprocesador) !! (addr-1)
    }

    cargarPrograma funcion microprocesador = microprocesador{
        programa = funcion
    }

    ejecutar microprocesador = ejecutarInstrucciones (programa microprocesador) microprocesador

    ejecutarInstrucciones instrucciones microprocesador = foldl ejecutarIntruccion microprocesador instrucciones

    ejecutarIntruccion microprocesador instruccion | ((=="").mensajeError) microprocesador = (incrementarProgramCounter.instruccion) microprocesador
                                                   | otherwise = microprocesador

    ifNZ instrucciones microprocesador | ((/=0).acumuladorA) microprocesador = ejecutarInstrucciones instrucciones microprocesador
                                       | otherwise = microprocesador

    depurar microprocesador instrucciones = filter (not.todoEn0.(flip ($) microprocesador)) instrucciones
    
    todoEn0 microprocesador = (acumuladorA microprocesador + acumuladorB microprocesador + sum (memoria microprocesador)) == 0
    
    verSiListaOrdenada [] = True
    verSiListaOrdenada [x] = True
    verSiListaOrdenada (x:y:ys) | x<=y = verSiListaOrdenada (y:ys)
                                | otherwise = False

    memoriaOrdenada microprocesador = verSiListaOrdenada (memoria microprocesador)



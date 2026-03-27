module Main (main) where

import Types
import Monad
import Parser
import Eval
import Theorem
import PP
import PrintChart
import System.Directory (doesFileExist) 

import System.IO (hFlush, stdout)

main :: IO ()
main = do 
    putStrLn "========================================="
    putStrLn "       DEMOSTRADOR DE TEOREMAS S5        "
    putStrLn "========================================="
    putStrLn "Elija una de las siguientes opciones:"
    putStrLn "  (1) Interfaz interactiva"
    putStrLn "  (2) Cargar un archivo con fórmulas"
    putStrLn "  (exit) Para terminar el programa"
    putStrLn "-----------------------------------------"
    putStr ">> "
    hFlush stdout
    s <- getLine
    putStrLn ""
    case s of
        "exit" -> return ()
        "1"    -> interactive
        "2"    -> fileReader
        _      -> do putStrLn "[!] Opción no válida. Intente nuevamente.\n"
                     main

interactive :: IO ()
interactive = do 
    putStrLn "-----------------------------------------"
    putStrLn "Modo Interactivo. Ingrese una fórmula"
    putStrLn "(o 'help' para ver sintaxis, o 'exit' para salir):"
    putStr ">> "
    hFlush stdout
    s <- getLine
    putStrLn ""
    case s of
        "exit" -> main
        "help" -> do showHelp
                     interactive  -- Lo cambié de 'main' a 'interactive' para no sacarlo del modo
        _      -> case parseForm "Syntax error" s of
                    Left e  -> do putStrLn $ "[ERROR DE SINTAXIS]\n" ++ show e ++ "\n"
                                  interactive
                    Right f -> do showTheorem f
                                  interactive

showHelp :: IO ()
showHelp = putStrLn $ unlines
    [ "================================================================="
    , "                    GUÍA DE SINTAXIS - LÓGICA S5                 "
    , "================================================================="
    , ""
    , " Constantes      : T (Verdadero)  |  F (Falso)"
    , " Variables       : Cualquier cadena de texto (ej. p, q, p1)"
    , " Negación        : ~P"
    , " Necesidad (Box) : []P"
    , " Posibilidad     : <>P"
    , " Conjunción (Y)  : P $$ Q"
    , " Disyunción (O)  : P || Q"
    , " Implicación     : P -> Q"
    , ""
    , " Precedencia: Unarios (~, [], <>) > Conjunción > Disyunción > Implicación."
    , " Se puede usar paréntesis '(' y ')' para agrupar operaciones."
    , "================================================================="
    , ""
    ]

fileReader :: IO ()
fileReader = do 
    putStrLn "-----------------------------------------"
    putStrLn "Ingrese el nombre de un archivo en 'examples/'"
    putStrLn "(o escriba 'exit' para volver al menú principal):"
    putStr ">> "
    hFlush stdout
    fp <- getLine
    putStrLn ""
    case fp of
        "exit" -> main
        _      -> let path = "examples/" ++ fp
                  in do exists <- doesFileExist path
                        if not exists 
                        then do putStrLn $ "[!] Error: El archivo '" ++ fp ++ "' no existe.\n"
                                fileReader
                        else do s <- readFile path
                                let forms = lines s
                                putStrLn $ "--- Archivo:" ++ fp ++ " ---\n"
                                processLines forms
                                fileReader

processLines :: [String] -> IO ()
processLines []     = return ()
processLines (l:ls) 
    | all (== ' ') l = processLines ls  -- Ignoramos líneas vacías
    | otherwise      = case parseForm "Syntax error" l of
                         Left e  -> do putStrLn $ "[ERROR DE SINTAXIS en la línea]: " ++ l
                                       putStrLn $ show e ++ "\n"
                                       processLines ls
                         Right f -> do showTheorem f
                                       processLines ls

showTheorem :: Form -> IO ()
showTheorem f = case checkTheorem f of
    Nothing     -> do putStrLn $ renderForm f ++ " es un teorema!\n"
    Just (t, w) -> do putStrLn $ renderForm f ++ " NO es un teorema."
                      if t == [] 
                      then putStrLn ""
                      else do putStrLn "--- Contraejemplo ---"
                              counterExample t w
                              putStrLn ""
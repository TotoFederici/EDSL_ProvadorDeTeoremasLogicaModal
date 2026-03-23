module Main (main) where

import Types
import Monad
import Parser
import Eval
import Theorem
import PP
import PrintChart
import System.Directory (doesFileExist) 

main :: IO ()
main = do putStrLn "Elije alguna de las siguientes opciones\n(1) Interfaz interactiva\n(2) Cargar un archivo con formulas\n(exit) Para terminar"
          s <- getLine
          putStrLn ""
          case s of
            "exit" -> return ()
            "1" -> interactive
            "2" -> fileReader
            _   -> do putStrLn "Esa no es una opcion valida\n"
                      main

interactive :: IO ()
interactive = do putStrLn "Ingrese una formula (o escriba 'exit' para terminar):"
                 s <- getLine
                 putStrLn ""
                 case s of
                    "exit" -> main
                    _      -> case parseForm "Syntax error" s of
                     Left e  -> do putStrLn $ show e ++ "\n"
                                   interactive
                     Right f -> do showTheorem f
                                   interactive

fileReader :: IO ()
fileReader = do putStrLn "Ingrese el nombre de un archivo del directorio 'examples' (o escriba 'exit' para terminar):"
                fp <- getLine
                putStrLn ""
                case fp of
                  "exit" -> main
                  _      -> let path = ("examples/" ++ fp)
                            in do exists <- doesFileExist path
                                  if not exists 
                                  then do putStrLn $ "Error: El archivo '" ++ fp ++ "' no existe.\n"
                                          fileReader
                                  else do s <- readFile path
                                          let forms = lines s
                                          processLines forms
                                          fileReader
processLines :: [String] -> IO ()
processLines []     = return ()
processLines (l:ls) 
        | all (== ' ') l = processLines ls  -- Si la línea está vacía o son puros espacios, la ignoramos y pasamos a la siguiente.
        | otherwise      = case parseForm "Syntax error" l of
                             Left e  -> do putStrLn $ show e ++ "\n"
                                           processLines ls
                             Right f -> do showTheorem f
                                           processLines ls

showTheorem :: Form -> IO ()
showTheorem f = case checkTheorem f of
                  Nothing     -> do putStrLn $ (renderForm f) ++ " es un teorema!\n"
                  Just (t, w) -> do putStrLn ((renderForm f) ++ " no es un teorema\n")
                                    if (t == []) then return () 
                                    else do putStrLn "Contraejemplo"
                                            counterExample t w
                                            putStrLn ""
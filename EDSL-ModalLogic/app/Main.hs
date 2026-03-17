module Main (main) where

import Types
import Monad
import Parser
import Eval
import Theorem
import PP
import PrintChart

main :: IO ()
main = do putStrLn "Elije alguna de las siguientes opciones\n(1) Interfaz interactiva\n(2) Cargar un archivo con formulas\n(exit) Salir"
          s <- getLine
          putStrLn ""
          case s of
            "exit" -> return ()
            "1" -> interactive
            "2" -> undefined
            _   -> do putStrLn "Esa no es una opcion valida\n"
                      main

interactive :: IO ()
interactive = do putStrLn "(exit) Para salir de la interfaz interactiva\nIngrese una formula"
                 s <- getLine
                 putStrLn ""
                 case s of
                    "exit" -> main
                    _      -> case parseForm "Syntax error" s of
                     Left e -> do putStrLn (show e)
                                  putStrLn ""
                                  interactive
                     Right f -> case checkTheorem f of
                                  Nothing     -> do putStrLn ((renderForm f) ++ " es un teorema!")
                                                    putStrLn ""
                                                    interactive
                                  Just (t, w) -> do putStrLn ((renderForm f) ++ " no es un teorema")
                                                    putStrLn ""
                                                    putStrLn "Contraejemplo"
                                                    counterExample t w
                                                    putStrLn ""
                                                    interactive
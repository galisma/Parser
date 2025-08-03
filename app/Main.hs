-- main
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Relude
import Parser

main :: IO ()
main = do
    r <- loadReplacements "palabrotas.txt" "sustitutos.txt"
    textoBS <- readFileBS "texto.txt"
    let texto = decodeUtf8 textoBS 
    let result = applyReplacements r texto
    putTextLn result
    num <- generateRandomNumber 100
    putStrLn $ "Número aleatorio: " <> show num
-- parser.hs
{-# LANGUAGE NoImplicitPrelude #-}

module Parser where
import Relude
import System.Random.Stateful

data Replacements = Replacements {slurs :: [Text], substitutes :: [Text]}

applyReplacements :: Replacements -> Text -> Text
applyReplacements replacements inputText = unwords outputWords
  where
    inputWords :: [Text]
    inputWords = words inputText
    outputWords :: [Text]
    outputWords = map doReplace inputWords
    doReplace :: Text -> Text
    doReplace inputWord =
      if inputWord `elem` slurs replacements
        then case substitutes replacements of 
               [] -> inputWord
               (x:_) -> x
        else inputWord

loadReplacements :: FilePath -> FilePath -> IO Replacements
loadReplacements pathPalabrotas pathSustitutos = do
    palabrotasBS <- readFileBS pathPalabrotas
    let palabrotasText = decodeUtf8 palabrotasBS 
    let palabrotas = words palabrotasText
    
    sustitutosBS <- readFileBS pathSustitutos
    let substitutosText = decodeUtf8 sustitutosBS 
    let sustitutos = words substitutosText
    
    pure Replacements { slurs = palabrotas, substitutes = sustitutos}

generateRandomNumber :: Int -> IO Int
generateRandomNumber n = randomRIO (1, n)



    {- 
    TODO: 
    - Usar un numero aleatorio (HECHO)
    - Implementar busqueda de arbol
    - Cambiar lista por un set
    -}
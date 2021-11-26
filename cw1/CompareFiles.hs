import System.Directory.Internal.Prelude (getArgs)
import Data.Char

data FileType = TAM
    deriving (Eq,Show)

{- compare content of two files -}
cmpFiles :: FilePath -> FilePath -> IO Bool
cmpFiles a b = do
    aContents <- readFile a
    bContents <- readFile b
    return (aContents == bContents)

{- Take only the file name -}
baseName :: String -> String
baseName = takeWhile (/='.')

{- Drop base name file name -}
fileExt :: String -> String
fileExt = dropWhile (/='.')

{- Use only tam files -}
extType :: String -> Maybe FileType
extType ".tam" = Just TAM
extType _ = Nothing

{- turn "abc.tam" into ("abc",TAM) -}
parseFileName :: String -> Maybe (String,FileType)
parseFileName arg = do
  if isAlpha (head arg)
    then
        let name = baseName arg
            ext  = extType (fileExt arg)
        in case ext of
            Just t -> Just (name,t)
            Nothing -> Nothing
    else
        Nothing

{- handling Maybe Type -}
unJust :: [Maybe a] -> [a]
unJust [] = []
unJust (Nothing:as) = unJust as
unJust (Just a:as) = a : unJust as

{- parse in files -}
fileNE :: [String] -> [(String,FileType)]
fileNE = unJust . (map parseFileName)

fileName s = takeWhile (/='/') (reverse s)

main :: IO ()
main = do
    args <- getArgs
    let parsed = fileNE args
    if length parsed == 2
        then
            do
                let (a:b:_) = parsed
                    (nameA, _) = a
                    (nameB, _) = b
                    result = cmpFiles (nameA ++ ".tam") (nameB ++ ".tam")
                boolResult <- result
                putStrLn ("Compare: " ++ nameA ++ " " ++ nameB ++ "  Result: " ++ show boolResult)
        else
            putStrLn "Require 2 tam files"
            
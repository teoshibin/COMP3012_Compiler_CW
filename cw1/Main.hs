{-

Compilers Course (COMP3012), 2021
    Venanzio Capretta
    Nicolai Kraus

Additional Code written by
    Shi Bin Teo

-}

{- 
    Main executable.

    A Compiler for MiniTriangle to TriangleAbractMachine
    Evaluater for MiniTriangle TriangleAbractMachine
-}

-- module Main where

-- import ExpParser
import Parser ( parseMT )
import Compiler ( compile2Str )
import TAM ( executeTAM, parseTAM, tam2String, traceTAM )

import System.Environment ( getArgs )
import Data.Char ( isAlpha )
import CodeGen ( codeGenAST )

{- If input file has extension .exp, compile the expression to TAM code
   If input file has extension .tam, execute tam code

   Options:
   --run : run the TAM code of the .exp source directly
           without generating a .tam file
   --evaluate : evaluate the .exp expression without generating TAM code
   --trace : trace the execution of .tam code
-}

data FileType
    = TAM
    | MT
    deriving (Eq,Show)

data Option
    = TraceStack
    | TraceParser
    | TraceAll
    | Run
    | Evaluate
    | Compile
    deriving (Eq,Show)



main :: IO ()
main = do
    args <- getArgs
    putStrLn ""

    -- let inputName = head args
    let (fileName,extension) = fileNE args
        ops = options args

    -- execute tam code and print result with 2 options with trace or without
    let executeTAMIO :: String -> IO ()
        executeTAMIO srcTam = do
            let tam = parseTAM srcTam
            putStrLn ""
            if TraceStack `elem` ops || TraceAll `elem` ops then do
                stk <- traceTAM tam
                putStrLn ("Final result: " ++ show (head stk))
            else do
                stk <- executeTAM tam
                putStrLn ("Stack: " ++ show stk)
                putStrLn ("Result: " ++ show (head stk))

    -- compile mt save it into tam and print AST if needed
    let compileIO :: String -> IO()
        compileIO srcMt = do
            if TraceParser `elem` ops || TraceAll `elem` ops then do
                let ast = parseMT srcMt
                    tam = codeGenAST ast
                    srcTam = tam2String tam
                putStrLn ("\nAbstract Syntax Tree: \n" ++ show ast ++ "\n")
                if null srcTam then
                    error "Failed to compile this file"
                else do
                    writeFile (fileName ++ ".tam") srcTam
                    putStrLn ("Compiled to: " ++ fileName ++ ".tam")
            else do
                let srcTam = compile2Str srcMt
                if null srcTam then
                    error "Failed to compile this file"
                else do
                    writeFile (fileName ++ ".tam") srcTam
                    putStrLn ("Compiled to: " ++ fileName ++ ".tam")

    -- main body of main function
    case extension of
        TAM -> do
            srcTam <- readFile (fileName++".tam")
            putStrLn ("Execute: " ++ fileName ++ ".tam")
            executeTAMIO srcTam
        MT -> do
            srcMt <- readFile (fileName ++ ".mt")
            putStrLn ("Loaded: " ++ fileName ++ ".mt")
            let mtdo
                    | Run `elem` ops = do
                        compileIO srcMt
                        srcTam <- readFile (fileName++".tam")
                        putStrLn ("Execute: " ++ fileName ++ ".tam")
                        executeTAMIO srcTam
                    | Evaluate `elem` ops = do
                        print "code for evaluate is incomplete"
                        -- putStrLn ("Evaluating Expression: " ++ show (evaluate (parseMT src)))
                    | otherwise = do
                        compileIO srcMt
            mtdo



{- 
    IO 
-}

-- Finding the base name and extension of a file name
baseName :: String -> String
baseName = takeWhile (/='.')

-- retrieve file extension
fileExt :: String -> String
fileExt fn =
    let ext = dropWhile (/='.') fn
    in if ext == "" then ".mt" else ext

-- convert extension type
extType :: String -> Maybe FileType
extType ".mt" = Just MT
extType ".tam" = Just TAM
extType _ = Nothing

-- parse filename into (basename, extension type)
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

-- input arguments
parseOption :: String -> Maybe Option
parseOption "--trace-stack" = Just TraceStack
parseOption "--trace-parser" = Just TraceParser
parseOption "--trace-all" = Just TraceAll
parseOption "--run" = Just Run
parseOption "--evaluate" = Just Evaluate
parseOption _ = Nothing

unJust :: [Maybe a] -> [a]
unJust [] = []
unJust (Nothing:as) = unJust as
unJust (Just a:as) = a : unJust as

options :: [String] -> [Option]
options = unJust . map parseOption

-- We assume one of the arguments is a file name
fileNE :: [String] -> (String,FileType)
fileNE = head . unJust . map parseFileName

{-
Compilers Course (COMP3012), 2021
  Venanzio Capretta
  Nicolai Kraus

A Compiler and evaluator for Arithmetic expressions with booleans,
relations, and conditional expressions.
Compiles simple Arith Expr into TAM programs.

Main executable.
-}

module Main where

-- import ExpParser
import MTParser
import MTCompiler
import TAM

import System.Environment
import Data.Char

{- If input file has extension .exp, compile the expression to TAM code
   If input file has extension .tam, execute tam code

   Options:
   --run : run the TAM code of the .exp source directly
           without generating a .tam file
   --evaluate : evaluate the .exp expression without generating TAM code
   --trace : trace the execution of .tam code
-}

data FileType = TAM | MT
    deriving (Eq,Show)

data Option = Trace | Run | Evaluate | Parse | Compile
    deriving (Eq,Show)

main :: IO ()
main = do
    args <- getArgs

    let inputName = head args
    let (fileName,extension) = fileNE args
        ops = options args
    let tamFun :: [TAMInst] -> IO ()
        tamFun = \tam ->
            if Trace `elem` ops
            then do stk <- traceTAM [] tam
                    putStrLn ("Final result: " ++ (show (head stk)))
            else putStrLn ("Executing TAM code: " ++ (show $ head $ execTAM [] tam))

    case extension of
        TAM -> do
            src <- readFile (fileName++".tam")
            let tam = parseTAM src
            tamFun tam
        -- EXP -> do
        --   src <- readFile (fileName++".exp")
        --   if Run `elem` ops
        --     then tamFun (compMT src)
        --     else if Evaluate `elem` ops
        --       then putStrLn ("Evaluating Expression: " ++ show (evaluate (expParse src)))
        --       else writeFile (fileName++".tam") (compileMTTAM src)
        --            >> putStrLn ("compiled to TAM file: " ++ fileName ++ ".tam")
        MT -> do
            src <- readFile (fileName++".mt")
            let dealWithMT
                    -- COMPILE & EXECUTE
                    | Run `elem` ops = tamFun (compMT src)
                    -- INTERPRET & EXECUTE
                    | Evaluate `elem` ops = do
                        print "code for evaluate not done yet"
                        -- TODO call interpreter
                        -- putStrLn ("Evaluating Expression: " ++ show (evaluate (mtParse src))) 
                    -- PRINT MT PRINT AST
                    | Parse `elem` ops = do
                        putStrLn "\n== Parser =="
                        putStrLn ("\nMT: \n\n" ++ src)
                        putStrLn ("\nAST: \n\n" ++ show (mtParse src))
                    -- COMPILE
                    | Compile `elem` ops = do
                        writeFile (fileName++".tam") (compileMTTAM src)
                        putStrLn ("compiled to TAM file: " ++ fileName ++ ".tam")
            dealWithMT

{- 
    IO 
-}

-- Finding the base name and extension of a file name

baseName :: String -> String
baseName = takeWhile (/='.')

fileExt :: String -> String
fileExt fn =
    let ext = dropWhile (/='.') fn
    in if ext == "" then ".mt" else ext

extType :: String -> Maybe FileType
extType ".mt" = Just MT
extType ".tam" = Just TAM
extType _ = Nothing

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

parseOption :: String -> Maybe Option
parseOption "--trace" = Just Trace
parseOption "--run" = Just Run
parseOption "--evaluate" = Just Evaluate
parseOption "--parse" = Just Parse
parseOption "--compile" = Just Compile
parseOption _ = Nothing


unJust :: [Maybe a] -> [a]
unJust [] = []
unJust (Nothing:as) = unJust as
unJust (Just a:as) = a : unJust as

options :: [String] -> [Option]
options = unJust . (map parseOption)

-- We assume one of the arguments is a file name
fileNE :: [String] -> (String,FileType)
fileNE = head . unJust . (map parseFileName)


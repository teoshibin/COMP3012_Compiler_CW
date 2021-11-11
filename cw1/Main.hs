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

import ExpParser
import ExpCompiler
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

data FileType = EXP | TAM
  deriving (Eq,Show)

data Option = Trace | Run | Evaluate
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
    EXP -> do
      src <- readFile (fileName++".exp")
      if Run `elem` ops
        then tamFun (compArith src)
        else if Evaluate `elem` ops
          then putStrLn ("Evaluating Expression: " ++ show (evaluate (expParse src)))
          else writeFile (fileName++".tam") (compileArithTAM src)
               >> putStrLn ("compiled to TAM file: " ++ fileName ++ ".tam")


-- Finding the base name and extension of a file name

baseName :: String -> String
baseName = takeWhile (/='.')

fileExt :: String -> String
fileExt fn = let ext = dropWhile (/='.') fn
             in if ext == "" then ".exp" else ext

extType :: String -> Maybe FileType
extType ".exp" = Just EXP
extType ".tam" = Just TAM
extType _ = Nothing

parseFileName :: String -> Maybe (String,FileType)
parseFileName arg = do
  if isAlpha (head arg)
    then let name = baseName arg
             ext  = extType (fileExt arg)
         in case ext of
              Just t -> Just (name,t)
              Nothing -> Nothing
    else Nothing

parseOption :: String -> Maybe Option
parseOption "--trace" = Just Trace
parseOption "--run" = Just Run
parseOption "--evaluate" = Just Evaluate
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


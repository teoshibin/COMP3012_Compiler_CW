{-

Compilers Course (COMP3012), 2021
    Venanzio Capretta
    Nicolai Kraus

Additional Code written by
    Shi Bin Teo
    
-}

{-
    Compiles simple Arith Expr into TAM programs.
-}

module Compiler where

import TAM ( tam2String, TAMInst )
import Parser ( parseMT )
import CodeGen ( codeGenAST )



{- 
    COMPILE FUNCTION
-}

-- Compiling MT Expressions to TAM
compile2Inst :: String -> [TAMInst]
compile2Inst = codeGenAST . parseMT

-- -- generate string to write to a file
compile2Str :: String -> String
compile2Str = tam2String . compile2Inst




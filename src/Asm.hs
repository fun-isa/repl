{-
//// THE SUPREME WU COMPILER 
//// VERSION 2.0  
//// AUTHOR: cecaccetti at sjtu edu cn
//// BATC
//// (C)2022 SHANGHAI JIAOTONG UNIVERSITY 
-}


{-HERE BE DRAGONS
这里有龙

PROCEED WITH CAUTION 
谨慎行事!!

-}



module Asm where

import FunISA
import Data.List
import Numeric
import Pattern

type Id = String
type AsmProgram =  [(Id, [AsmInsn])] 

data AsmInsn = Root 
             | Add Bool
             | Sub Bool
             | Mul Bool
             | Div Bool
             | Rem Bool
             | Eq Bool
             | Gt Bool
             | Lt Bool
             | Srl Bool
             | Sra Bool
             | Sll Bool
             | Xor Bool
             | Or Bool
             | And Bool
             | Addi Int Bool
             | Xori Int Bool
             | Slli Int Bool
             | Srai Int Bool
             | Srli Int Bool
             | Andi Int Bool
             | Ori Int Bool
             | Muli Int Bool
             | Mulhi Int Bool
             | Divi Int Bool
             | Remi Int Bool
             | Maddi Int Bool
             | Eqi Int Bool
             | Gti Int Bool
             | Lti Int Bool
             | Cata Bool
             | Ana Bool
             | Hylo Bool
             | Para Bool
             | Apo Bool
             | Histo Bool
             | Par Bool
             | PushEval Int
             | Pusharg1 Int
             | Pusharg2 Int Int
             | Pusharg3 Int Int Int
             | Pusharg4 Int Int Int Int
        --     | MkThunk CType Int Int Int Int Int
             | Closure Int Int [Int] Bool
             | Combi Type Int [Int] Bool
             | Fork
             | Out Bool 
             | ForceEval
             | Lambda Int Int
             | AsmLink Id Bool Bool
             | Break Bool
             | AsmLit Int
             | AsmExt Int Bool Bool
             | Block Int Bool  
             deriving(Show,Eq)
          


getCType :: String -> Type
getCType "T0" = T0 
getCType "T1" = T1
getCType "T2" = T2
getCType "T3" = T3
getCType "T4" = T4
getCType "T5" = T5
getCType "T6" = T6
getCType "T7" = T7
getCType "T8" = T8
getCType "T9" = T9
getCType "T10" = T10
getCType "T11" = T11
getCType "T12" = T12
getCType "T13" = T13
getCType "T14" = T14
getCType "T15" = T15
getCType "T16" = T16
getCType "T17" = T17
getCType "T18" = T18
getCType "T19" = T19
getCType "T20" = T20
getCType "T22" = T21
getCType "T23" = T22
getCType "T24" = T23
getCType "T25" = T24
getCType "T26" = T25
getCType "T27" = T26
getCType "T28" = T27
getCType "T29" = T28
getCType "T30" = T29
getCType "T31" = T30
getCType "T32" = T31
getCType "T33" = T32
getCType "T34" = T33
getCType "T35" = T34
getCType "T36" = T35
getCType e  = error("Type " ++ (show e) ++ "not defined")

patchZeros :: [Int] -> [Int]
patchZeros [] = error("Empty combinator")
patchZeros ls = case (length ls) of
               1 -> ls ++ [0,0,0,0,0,0,0]
               2 -> ls ++ [0,0,0,0,0,0]
               3 -> ls ++ [0,0,0,0,0]
               4 -> ls ++ [0,0,0,0]
               5 -> ls ++ [0,0,0]
               6 -> ls ++ [0,0]
               7 -> ls ++ [0]
               8 -> ls
               
patch6Zeros :: [Int] -> [Int]
patch6Zeros [] = error("Empty combinator")
patch6Zeros ls = case (length ls) of
               1 -> ls ++ [0,0,0,0,0]
               2 -> ls ++ [0,0,0,0]
               3 -> ls ++ [0,0,0]
               4 -> ls ++ [0,0]
               5 -> ls ++ [0]
               6 -> ls 
      

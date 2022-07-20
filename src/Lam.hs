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


module Lam where

import Control.Monad
import Control.Applicative
import Data.List
import Data.Char
import Asm 
import Pattern

type Program =  Stm 
                 
type VarList =  [Id] 
type ParList =  [Exp] 

data Stm = Fun Id Exp
         | Sin Exp
         deriving (Eq, Show)

data Exp =  App Exp Exp
         |  ApL Exp [Exp]
         |  LVar String Exp
         |  Lam Int Exp
         |  Num Int
         |  Sym String 
         |  Idx Int 
         |  C Type Int [Int]
         deriving(Show,Eq) 
         
data Op = Add
        | Sub
        | Mul
        | Div
        deriving (Show, Eq)
                    
               
data Pattern = Patt Type Exp deriving (Show, Eq)


--Make lambda abstraction from body and variable list
mkAbs :: Exp -> VarList -> Exp
mkAbs ex [] = ex
mkAbs ex ((x):xs) = mkAbs (LVar x ex) xs  

--Convert "string" to [s,t,r,i,n,g]
mkString :: String -> Exp
mkString [] = ( (Sym "Nil"))
mkString (x:xs) = App (App ( (Sym "Cons")) ( (Num (ord x)   ))) (mkString xs)

-------------------

pprogram :: [Program] -> String
pprogram [] = []
pprogram (x:xs) = pprint x ++ "\n" ++ pprogram xs

pprint :: Program -> String
pprint (Fun id exp) = id ++ " = " ++ (ppExp exp)
pprint (Sin exp) = ppExp exp

ppExp :: Exp -> String
ppExp (App e1 e2) = (ppExp e1) ++ "(" ++ (ppExp e2) ++ ")"
ppExp (Sym s) = s
ppExp (Num n) = show n
ppExp (C t a l) = "C" ++ (show a) ++ (show t) ++ (show (map (\x -> x - 1)l))
ppExp e = error (show e)











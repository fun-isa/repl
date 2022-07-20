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


module Frontend where

import Control.Monad
import Control.Applicative
import Data.List
import Data.Char
import Lam
import Asm
import Pattern

-- lambda lifting
liftAdjust = runAdjust . runLift

runLift ::  [Program] -> [Program]  
runLift [] = []
runLift ((Fun id exp) :xs) = [(Fun id (goLift id exp))] ++ (runLift ((goDetach id exp)++xs))  
runLift ((Sin exp) :xs) = [(Sin (goLift "id" exp))] ++ (runLift ((goDetach "id`" exp)++xs))  

goLift :: Id ->  Exp -> Exp
goLift id (App e1 e2) =  App (goLift (id++"_l") e1) (goLift (id++"_r") e2)
goLift id (Lam n e) = Lam n (liftAbs id e)
goLift _ e = e


runAdjust ::  [Program] -> [Program]  
runAdjust [] = []
runAdjust ((Fun id exp):xs) = [Fun id (goAdjust exp)] ++ (runAdjust xs)  
runAdjust ((Sin exp):xs) = [Sin (goAdjust exp)] ++ (runAdjust xs)  

goDetach :: Id ->  Exp -> [Program]
goDetach id (App e1 e2) =   (goDetach (id++"_l") e1) ++ (goDetach (id++"_r") e2)
goDetach id (Lam n e) = detachAbs id e 
goDetach _ _ = []

detachAbs :: Id ->  Exp -> [Program]
detachAbs id (App e1 e2) =   (detachAbs (id++"_l") e1) ++ (detachAbs (id++"_r") e2)
detachAbs id (Lam n e) = [Fun id (Lam n e)]
detachAbs _ _ = []

liftAbs :: Id ->  Exp -> Exp
liftAbs id (App e1 e2) =  App (liftAbs (id++"_l") e1) (liftAbs (id++"_r") e2)
liftAbs id (Lam n e) = appFreeIdx (Sym id) (sort(nub( (freeIdxs n (liftAbs (id++"#") e)) )))
liftAbs _ e = e

appFreeIdx :: Exp -> [Int] -> Exp
appFreeIdx e [] = e
appFreeIdx e (x:xs) = appFreeIdx (App e (Idx x)) xs

freeIdxs :: Int -> Exp -> [Int]
freeIdxs n (App e1 e2) = (freeIdxs n e1 ) ++ (freeIdxs n e2)
freeIdxs n (Idx m) | m > n = [ (m - n)]
                   | otherwise = []
freeIdxs _ e = []

goAdjust :: Exp -> Exp
goAdjust (App e1 e2 ) = App (goAdjust e1) (goAdjust e2)
goAdjust (Lam n e) = Lam (len + n) (adjustIdx lst n e) where len = length lst 
                                                             lst = (sort(nub(freeIx n e)))
goAdjust e = e

adjustIdx :: [Int] -> Int -> Exp -> Exp
adjustIdx len n  (App e1 e2)  = (App (adjustIdx len n e1) (adjustIdx len n e2)  )
adjustIdx len n (Idx x) | (x <= n) = (Idx (x+ (length len))) 
                        | otherwise =  (Idx (findIdx x len)) 
adjustIdx _ _ e = e

findIdx ::  Int -> [Int] -> Int
findIdx n ls = findIdx_ n ls 1

findIdx_ :: Int -> [Int] -> Int -> Int
findIdx_ _ [] _ = error("idx not found")
findIdx_ n (x:xs) c | n == x = c
                    | otherwise = findIdx_ n xs (c+1) 

freeIx :: Int -> Exp -> [Int]
freeIx n (App e1 e2) = (freeIx n e1 ) ++ (freeIx n e2)
freeIx n (Idx m) | m > n = [ (m )]
                 | otherwise = []
freeIx _ e = [] 


---------------------------------------------------------------------

-- From lambda with variables to de Bruijn 

convert :: Program -> [Program] 
convert (Fun id exp) = [Fun id (  (compactLam (e2db exp)))]
convert (Sin exp) = [Sin ( (compactLam (e2db exp)))]

e2db :: Exp -> Exp
e2db (App a b)  = App (e2db a)(e2db b)
e2db (LVar id e) = Lam 1 (dbracket 1 id (e2db e))
e2db e = e

dbracket :: Int -> String -> Exp -> Exp
dbracket n var (App a b) = App (dbracket n var a) (dbracket n var b)
dbracket n var (Sym x) | x == var = Idx n
                       | otherwise = (Sym x)
dbracket n var (Lam idx e) = Lam idx (dbracket (n + 1) var e)
dbracket _ _ (Idx x) = (Idx x)  
dbracket _ _ e = e 

compactLam :: Exp -> Exp
compactLam (App e1 e2) = App (compactLam e1) (compactLam e2)
compactLam (Lam id1 (Lam id2 e)) = compactLam  (Lam (id1 + id2) (compactLam e))
compactLam (Lam id e) = Lam id (compactLam e)
compactLam e =  e
















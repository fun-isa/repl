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



module Backend where

import Control.Monad
import Control.Applicative
import Data.List
import Data.Char
import Lam
import Bracket
import Asm
import Pattern


l2combi :: Program -> Program
l2combi (Fun id e) = Fun id (lam2combi e)
l2combi (Sin e) = Sin (lam2combi e)


lam2combi :: Exp -> Exp 
lam2combi (Lam n e)  =  case(match super ) of
                       Just x  -> elimId(appAtom x constants) 
                       Nothing -> iterateBracket (bracket') n e
                       where  (super, constants) = liftAtom (Lam n  e)  
lam2combi (App e1 e2) = (App (lam2combi e1) (lam2combi e2))
lam2combi e = e 


------------  Atom Lifting

isAtom :: Exp -> Bool
isAtom (Sym o) = True
isAtom (Num l) = True
isAtom e = False 

liftAtom :: Exp -> (Exp, [Exp])
liftAtom ex = case (len == 0) of
              True -> (ex,[])
              False ->     ((  subsAtom (incIn  ex len) freelist     ) , freelist) 
            where freelist =  (freeAtoms ex)
                  len = length freelist

subsAtom :: Exp -> [Exp] -> Exp
subsAtom (App e1 e2) ls = App (subsAtom e1 ls)  (subsAtom e2 ls)
subsAtom (Lam n e) ls = Lam n (subsAtom e ls)
subsAtom e ls | isAtom e = Idx (lpos e ls)
              | otherwise = e  

appAtom :: Exp ->  [Exp] -> Exp
appAtom e [] = e
appAtom e (x:xs) = appAtom (App e x) xs

incIn :: Exp -> Int -> Exp
incIn (App e1 e2) l = App (incIn e1 l) (incIn e2 l)
incIn (Lam n e) l = Lam (n+l) (incIn e l)
incIn (Idx n) l = Idx (n+l)
incIn e l = e

freeAtoms = nub . freeAtoms'

freeAtoms' :: Exp -> [Exp]
freeAtoms' (App e1 e2) = (freeAtoms' e1) ++ (freeAtoms' e2)
freeAtoms' (Lam n e) = freeAtoms' e
freeAtoms' e | isAtom e = [e]
             | otherwise = []

lpos :: Exp -> [Exp] -> Int
lpos l ls = lpos' l ls 1

lpos' :: Exp -> [Exp] -> Int -> Int
lpos' _ []  _ = error ("atom not on freelist")
lpos' l (x:xs) n | l == x =  n
                 | otherwise = lpos' l xs (n+1)


----------- Pattern Matching

match (Lam n e) = toCombi e n

toCombi :: Exp -> Int -> Maybe Exp
toCombi e a | a < 9 =  case (matchPattern e patternList)of
                        Nothing -> Nothing
                        Just (Patt t0 f) -> Just (C t0 a (traverseExp e))
            | otherwise = Nothing

projectList :: Pattern -> Exp
projectList (Patt _ xs) = xs

projectType :: Pattern -> Type
projectType (Patt t _) = t

matchPattern :: Exp -> [Pattern] -> Maybe Pattern
matchPattern exp  [] = Nothing
matchPattern exp (x:xs) | matchExp exp pj = (Just x) 
                         | otherwise = matchPattern exp xs  where pj = (projectList x)

matchExp :: Exp -> Exp -> Bool
matchExp (App (Idx _) (Idx _)) (App (Idx _) (Idx _)) = True
matchExp (App (Idx _) e1) (App (Idx _) e2) = matchExp e1 e2
matchExp (App e1 (Idx _)) (App e2 (Idx _)) = matchExp e1 e2
matchExp (App e1 e2) (App e3 e4) = (matchExp e1 e3) && (matchExp e2 e4)
matchExp (Idx _ ) (Idx _) = True
matchExp e1 e2 = False  


adjustIn :: Int -> Exp -> (Int, Exp)
adjustIn n (App e1 e2) = (fst ee2 , App (snd ee1) (snd ee2) ) where ee1 = adjustIn n e1  
                                                                    ee2 = adjustIn (fst ee1) e2   
adjustIn n (Idx m) = (n+1 , Idx n)

patternList = (Patt Tx (Idx 1)) :  (mkPatt 0 (map (snd .(adjustIn 1)) (concat (tail(take 6 sizes))) ))


mkPatt :: Int -> [Exp] -> [Pattern]
mkPatt _ [] = []
mkPatt n (x:xs) = (Patt (mkType n) x ) : (mkPatt (n+1) xs)  

sizes :: [[Exp]]
sizes = [Idx 0] : (map go . drop 1 . inits) sizes  where
    go smaller = do
      (ls, rs) <- zip smaller (reverse smaller)
      liftM2 App ls rs

traverseExp :: Exp -> [Int]
traverseExp (App e1 e2) = (traverseExp e1) ++ (traverseExp e2)
traverseExp (Idx x) = [x]

elimId :: Exp -> Exp
elimId (App (C Tx 1 [1]) e ) = e
elimId (App (C T0 2 [1,2]) e ) = e
elimId (App (C T2 3 [1,2,3]) e) = e
elimId (App (C T7 4 [1,2,3,4]) e) = e
elimId (App (C T21 5 [1,2,3,4,5]) e) = e
elimId (App (C T63 6 [1,2,3,4,5,6]) e) = e
elimId (App e1 e2) = App (elimId e1) (elimId e2)
elimId e = e


------ Unification


mapApl ::  (Exp -> Exp) -> Exp -> Exp
mapApl f (ApL e ls) = ApL e (map f ls)
mapApl _ e = e  

combine :: [Exp]  ->  Exp  ->  Maybe Exp
combine ps (ApL (C t a l) ls) | len >= a =  combine ps  e -- subs ps
                              | len < a =   combine (drop (a - len) ps ) ee -- subs (drop (a - len) ps )
                          
                           where e =  mapApl (subsInternal ) (flatten (drop a ls) (typeEval t patternList  ls l))
                                 ee =  mapApl (subsInternal ) (flatten [] (typeEval t patternList  (ls++(take (a - len) (ps) )) l))
                                 len = length ls
combine ps (C t a l) = combine (tail ps) (ApL (C t a l) [(head ps)])
combine ps e =  toCombi (unflatten e) (getIndex (head ps) - 1)


idxs :: Int -> [Exp]
idxs n =  (Idx n)  : idxs (n+1)


idxs2 :: Int ->  Int -> [Exp]
idxs2 m n | m >= n  = (Idx n)  : (idxs2 m (n+1))
          | otherwise = []




unify (Fun id e) = Fun id ((unflatten . unify2 . (flatten [])) e)
unify (Sin e) = Sin ((unflatten . unify2 . (flatten [])) e)




unify2 :: Exp -> Exp
--unify2 (App e1 Roots) = App (unify2 e1) Roots
unify2 (ApL (C t a l) ls ) = case (len >= a ) of
                             True -> unify2 (redux (ApL (C t a l) ls ))
                             False -> case ls of
                                    [] ->   ((C t a l) )
                                    _ -> case (head lss) of
                                           (C t2 a2 l2) -> case (combine (idxs 1) (ApL (C t a l) [C t2 a2 l2]) ) of
                                                      Nothing -> ApL (C t a l) (lss) 
                                                      Just x -> unify2(ApL x (tail lss))
--                                           
                                           (ApL (C t2 a2 l2) lz) -> case (combine (idxs ((length lz) + 1)) (ApL (C t a l) [ApL (C t2 a2 l2) (idxs2 (length lz) 1  )] ) )  of
                                                                 Nothing -> ApL (C t a l) (lss)
                                                                 Just x ->   (ApL x (lz ++ (tail lss)))
                                                                                                             
                                           _ -> ApL (C t a l) lss
                                         where lss = map unify2 ls
                             where len = length ls         
unify2 (ApL e ls) = ApL e (map unify2 ls)
unify2 e = e    

redux ::  Exp  -> Exp
redux (ApL (C t a l) ls)   | len >= a =  redux e -- subs ps
                           | otherwise = (ApL (C t a l) (map redux ls)) 
                           where e =  mapApl (subsInternal ) (flatten (drop a ls) (typeEval t patternList  ls l))
                                 len = length ls
redux  e = e

subsInternal ::   Exp  -> Exp
subsInternal  (ApL (C t a l) ls) | len >= a =  subsInternal  e -- subs ps
                                 | otherwise = (ApL (C t a l) ls)
                           where e =  mapApl (subsInternal) (flatten (drop a ls) (typeEval t patternList  ls l))
                                 len = length ls
subsInternal  e = (e)

typeEval :: Type -> [Pattern] -> [Exp] -> [Int]-> Exp
typeEval tp ts ls pl =  typeEval_ (getPattEx tp ts) ls pl  

typeEval_ :: Exp -> [Exp] -> [Int] -> Exp
typeEval_ (App e1 e2) ls pl = App (typeEval_ e1 ls pl  ) (typeEval_ e2 ls pl)
typeEval_ (Idx n) ls pl =  (unflatten(ls!!((pl!!(n-1))-1)))

flatten :: [Exp] -> Exp -> Exp
flatten ls (App e1 e2)  = flatten  ((flatten [] e2) :ls) e1
flatten ls (ApL (ApL a l1) l2) = ApL a (l1 ++ l2)
flatten ls (ApL a []) = a
flatten [] e = e
flatten  ls e  = ApL e ls

getIndex (Idx x) = x

getPattEx :: Type -> [Pattern] -> Exp
getPattEx _ [] = error("Pattern not found")
getPattEx t ((Patt tt exp):xs) | tt == t  = exp
                               | otherwise = getPattEx t xs  

unflatten :: Exp -> Exp
unflatten (ApL e ls) = mkAp e (map unflatten ls)
unflatten e = e 


mkAp :: Exp -> [Exp] -> Exp
mkAp e [] = e 
mkAp e (x:xs) = mkAp (App e x) xs

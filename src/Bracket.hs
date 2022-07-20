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


module Bracket where


import Lam
import Asm
import Pattern


---------- Bracket Abstraction

-- Basic combinators
s = C T5 3 [1,3,2,3]
k = C Tx 2 [1]
i = C Tx 1 [1]
b = C T1 3 [1,2,3]
c = C T2 3 [1,3,2]
d = C T5 4 [1,2,3,4]
c1 = C T6 4 [1,2,4,3]
s1 = C T15 4 [1,2,4,3,4]


iterateBracket :: (Int -> Exp -> Exp) -> Int -> Exp -> Exp
iterateBracket _ 0 e = e
iterateBracket f n e = iterateBracket f (n -1)  (f n e)


freeIdx :: Int -> Exp -> Bool
freeIdx n (Idx x) | x == n = False
                  | otherwise = True
freeIdx n (App e1 e2) = (freeIdx n e1) && (freeIdx n e2)
freeIdx _ _ = True


bracket' :: Int -> Exp -> Exp
bracket' n (App e1 (Idx x)) | (freeIdx n e1) && (n == x) = e1
                          | (freeIdx n e1) && (n /= x) = App k (App e1 (Idx x))
                          | ((freeIdx n e1) == False) && (x /= n) =  App (App c (bracket' n e1)) (Idx x)
                          | otherwise = App (App s (bracket' n e1)) (bracket' n (Idx x))
bracket' n (App (App e1 (Idx x)) e2) | (freeIdx n e1) && (freeIdx n e2) && (n == x) = App (App c e1 ) e2
                                  | (freeIdx n e1) && ((freeIdx n e2)==False ) && (n == x) = App (App s e1 ) (bracket' n e2)
bracket' n (App (App e1 e2)e3) | (freeIdx n e1) && (freeIdx n e2) && (freeIdx n e3)  = App k (App(App e1 e2)e3)
                             | (freeIdx n e1) && (freeIdx n e2) = App (App (App d e1) e2)  (bracket' n e3)
                             | (freeIdx n e1) && (freeIdx n e3) =  App (App ( App c1 e1) (bracket' n e2)) e3
                             | (freeIdx n e1) = App (App (App s1 e1) (bracket' n e2)) (bracket' n e3) 
                        --     | otherwise = App(App(App s2 (bracket' n e1) ) (bracket' n e2) ) (bracket' n e3) 
bracket' n (App e1 e2)  --  | (freeIdx n e1) && (freeIdx n e2) = App k (App e1 e2)
                     | (freeIdx n e2) = App (App c (bracket' n e1)) e2
                     | (freeIdx n e1) = App (App b e1 ) (bracket' n e2)
                     | otherwise =  (App( (App s (bracket' n e1 ))) (bracket' n e2 ))
bracket' n (Idx x) | n /= x = App k (Idx x)
                   | n == x = i
bracket' n (C t a l) = C t (a+1) (map (+ 1) l)
bracket' n e =  (App k e)




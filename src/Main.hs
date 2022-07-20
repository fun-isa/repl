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


import qualified LamParser
import Control.Monad (unless) 
import System.IO
import Lam
import Frontend
import Backend
import Bracket


parse :: String -> Program
parse = LamParser.parseExpr

main :: IO ()
main = repl []

repl :: [Program] -> IO ()
repl ls = do
        input <- read'
        
        let env = case input of
                  ":env" -> ls
                  ":q" -> ls
                  _ -> (ls ++ ( (compile input)))
        
        unless (input == ":q")
           $ cases env (input) 
          >> repl env 
         
read' :: IO String
read' = putStr "wu> "
     >> hFlush stdout
     >> getLine         
           
eval' :: String -> [Program]
eval' input = compile input

compile = ( (map (unify . l2combi)). liftAdjust . convert . parse )
 --   pprint . convert . liftLambdas . l2db .  parse

print' = putStrLn
           

cases :: [Program] -> String -> IO ()
cases env str = case str of
               ":env" -> print' (show (map pprint  env))
               _ ->  print' (pprogram(eval' str))


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

{
module LamParser where

import LamToken
import Lam
import Numeric
import Data.Char
import Data.List
}

%name expr
%tokentype { Token }
%error { parseError }


%token

 NUM   { TokenNum $$ }
 NEG   { TokenNeg $$ }
 FLOAT { TokenFloat $$ }
 ID   { TokenSym $$ }
 STR  { TokenStr $$ }
 HEX  { TokenSymH $$}
 CHAR { TokenChar $$}
 CON  { TokenCon $$}
 let                            { TokenLet }
 '#'                            { TokenPound } 
 '->'                          { TokenArrow }    
 '='                           { TokenAssign }
 '|'                           { TokenOr }
 '('                           { TokenLParen }
 ')'                           { TokenRParen }
 '['                         { TokenLBrac }
 ']'                         { TokenRBrac  }
 '{'                         { TokenLBrace }
 '}'                         { TokenRBrace  }
 '"'                         { TokenQuote }
 ','                         { TokenComma }
 ';'                         { TokenSemi }
 ':'                         { TokenColon }
 '\\'                        { TokenLambda}
 
%left expression
%left ID NUM
%nonassoc APP
%%



program 
       :  statement { $1}



statement
        : function_definition   {$1}
        | let singleton {$2}
       

singleton: fact {Sin $1}

function_definition:  ID sym_list '=' body2   {Fun $1 ( mkAbs $4 ( $2))}

param_list :   {[]}
           | ID param_list {$1:$2}

body : body2     {$1}
     

body2: '\\' var_list '->' body2  { mkAbs $4 (($2)) }
     | fact {$1}

fact : fact atom   {(App $1 $2)}
     | atom {$1}
     
atom : '(' body2 ')'    {$2} 
     |   terminals             {$1}
    
var_list:               {[]}
        |  ID var_list { $1 : $2  }
        
sym_list:               {[]}
        |  ID sym_list { $1 : $2  }

symbol :  ID {Sym $1}

terminals 
	: '[' list ']'       {( $2)}
	| symbol     {$1}
	| negliteral {$1}
        | STR        { mkString (tail (init $1)) }   
        | literals           {  $1 }
                
list : atom      {App (App (Sym "Cons") $1) (Sym "Nil")}        
     | atom ',' list  {App (App (Sym "Cons") $1) $3 } 

negliteral : NEG     {(Num $1)} 	

literals :  NUM            {(Num $1)}
         |  CHAR           {(Num(ord(head(tail $1))))}
         | HEX             { (Num ((fst (head( readHex(tail (tail $1)))))))} 
         
{

errrrr e = error (show  e) 

tkStr :: Token -> [Char]
{-
tkStr TokenLet = "let "
tkStr (TokenNeg n) = show n
tkStr (TokenNum n) = (show n) ++ " "
tkStr (TokenFloat f) = (show f) ++ " "
tkStr (TokenSym s) = s ++ " "
tkStr (TokenStr s) = s ++ " "
tkStr (TokenChar s) = s ++ " " 
tkStr TokenArrow = "-> " 
tkStr TokenCase = "case "
tkStr TokenIn  = "at "
tkStr TokenImport = "import "
tkStr TokenExtern = "extern "
tkStr TokenAll = "_"
tkStr TokenSemi = "; "
tkStr TokenComma = ","
tkStr TokenQuote = "\""
tkStr TokenAssign = "= "
tkStr TokenLParen = "("
tkStr TokenRParen = ")"
tkStr TokenLBrac = "["
tkStr TokenRBrac = "]"
tkStr TokenDot = "."
tkStr TokenLambda = "\\"
tkStr (TokenSymH s) = s ++ " "
-}
tkStr x = show x

showTk :: [Token] -> [Char]
showTk xs = intercalate "" (map tkStr xs)

parseError :: [Token] -> a
parseError tk = error ("Parse error at: " ++ (showTk tk) ++ "\n")

parseExpr :: String -> Program
parseExpr = expr . scanTokens
}




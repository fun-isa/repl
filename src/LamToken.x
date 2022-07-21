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
{-# OPTIONS_GHC -w #-}
module LamToken (Token(..),scanTokens) where
import Lam
}

%wrapper "basic"

$hexdigit =[0-9a-fA-F]
$dot = [.]
$flt = [\$]
$digit = 0-9
$alpha = [a-zA-Z]
$lower = [a-z\+\-\*\>\<\|\&\/\%\.\[\]\!\^]
$upper = [A-Z]
$sym = [\+\-]
$sym2 = [\*\>\<\=\|\&\/\%\.\:\~\@\^]
$eol   = [\n]
$char = [a-zA-z]
$sstring = [0-9a-zA-Z\ \^\+\-\\\*\>\<\=\:\|\&\/\%\_\.\$\,\@\!\?\\-\_\~\;\(\)\$\^\{\}\[\]\`]

tokens :-

  $white+                       ;
  "--".*                        ;
  "{-"[$hexdigit $white $digit $alpha $sym $char $sstring]* "-}" ; 
  let                           { \s -> TokenLet }
  \( \- $digit+ \)                {\s  -> TokenNeg ((read s)) }
  $flt $digit+ $dot $digit+      {\s  -> TokenFloat ((read (tail s))) }
  $digit+                       { \s -> TokenNum (read s) }
  \#                              { \s -> TokenPound } 
  "->"                            { \s -> TokenArrow }      
  \=                            { \s -> TokenAssign }
  \|                            { \s -> TokenOr }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
   \[                            { \s -> TokenLBrac }
   \]                            { \s -> TokenRBrac }
  \{                         { \s -> TokenLBrace }
 \}                         { \s -> TokenRBrace  }
  \" ([$sstring $white])* \"    { \s -> TokenStr s}
  \' ($sstring)* \'       { \s -> TokenChar s }
  \;                       {\s ->  TokenSemi}
  asm                           {\s -> TokenAsm}   
  import 		        {\s -> TokenImport}
  extern 		        {\s -> TokenExtern}
  case	                        {\s -> TokenCase}
  of	                        {\s -> TokenOf}
  data                        {\s -> TokenData}
  RULE                        {\s -> TokenRule}
  where                         {\s -> TokenWhere}
  in                         {\s -> TokenIn}
  \_                            {\s -> TokenAll}
  \@                            {\s -> TokenAt}
  \,                            { \s -> TokenComma }    
  \\                            {\s -> TokenLambda}
  \:                            {\s -> TokenColon }
  "0x" [$hexdigit]+              { \s -> TokenSymH s }  
  
  
  
  $lower [$alpha $digit \_ \']*      { \s -> TokenSym s }
  $upper [$alpha $digit \_ \']*      { \s -> TokenCon s }


{

data Token = TokenLet
           | TokenBinOp String
           | TokenBinOp2 String
           | TokenNum Int
           | TokenFloat Float
           | TokenSym String
           | TokenCon String 
           | TokenStr String
           | TokenChar String
           | TokenNeg Int
           | TokenPound
           | TokenArrow
           | TokenCase
           | TokenImport
           | TokenAll
           | TokenDo
           | TokenWhere
           | TokenData
           | TokenRule
           | TokenComma
           | TokenQuote
           | TokenIn
           | TokenAt
           | TokenOr
           | TokenOf 
           | TokenAsm 
           | TokenAssign      
           | TokenLParen
           | TokenRParen
           | TokenLBrac
           | TokenRBrac
           | TokenLBrace
           | TokenRBrace
           | TokenDef 
           | TokenColon
           | TokenSemi
           | TokenEOL
           | TokenDot
           | TokenLambda
           | TokenExtern
           | TokenSymH String
            deriving (Eq,Show)

scanTokens = alexScanTokens

}

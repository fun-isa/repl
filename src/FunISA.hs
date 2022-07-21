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

module FunISA where


data Insn = SymLink String Bool Bool
          | ImmLink Int Bool Bool
          | TLoc Int
          | LType Int
          | Blk Int Bool 
          | Roots 
          | OpI   Opcode Funct5 Int Bool
          | OpII  Opcode Int Int Int Int Int Funct2 Bool
          | OpIII Opcode Funct5 Int Bool
          | OpIV  Opcode Funct5 Int Int Bool
          | OpV  Opcode Int Int Int Int Int Int Int Int Int Bool
          | OpVI Opcode Funct5 Int Int Int Int Bool
          | OpVII Opcode Funct6 Int Int Int Int Int Int Int Bool
          deriving(Show,Eq,Read)
          
          
data Opcode = Mgt
            | Comb
            | Cls
            | Arg
            | Alu
            | AluImm
            | MulDiv
            | MulDivImm
            | Csr
            | Fpu
            | Morph
            | Xtra
            deriving(Show, Eq, Read)
            
data Funct5 = F5Add
            | F5Sub
            | F5Sll
            | F5Xor
            | F5Srl
            | F5Sra
            | F5Or
            | F5And
            | F5Mul
            | F5Mulh
            | F5Mulhsu
            | F5Mulhu
            | F5Div
            | F5Divu
            | F5Rem
            | F5Remu
            | F5Madd
            | F5Msub
            | F5Eq
            | F5Gt
            | F5Lt
            | F5Csrr
            | F5Csrrw
            | F5Csrwi
            | F5Csrrwi
            | F5Addi
            | F5Slli
            | F5Xori
            | F5Srli
            | F5Srai
            | F5Ori
            | F5Andi
            | F5Muli
            | F5Mulhi
            | F5Divi
            | F5Remi
            | F5Maddi
            | F5Eqi
            | F5Gti
            | F5Lti
            | F5Fadd
            | F5Fsub
            | F5Fmul
            | F5Fdiv
            | F5Fsqrt
            | F5Fti
            | F5Itf
            | F5Fmac
            | F5Feq
            | F5Flt
            | F5Fgt
            | F5Cata
            | F5Ana
            | F5Hylo
            | F5Para
            | F5Apo
            | F5Histo
            | F5Out
            | F5Fork
            | F5ForceEval
            | F5Par
            | F5PushEval
            | F5PushArg1
            | F5PushArg2
            | F5PushArg3
            | F5PushArg4
            | F5Break
            | F5Root
            | F5Lam
            | F5Fix
            deriving(Show, Eq, Read)

data Funct2 = Low
            | High
            deriving(Show, Eq, Read)

op2hex :: Opcode -> Integer
op2hex Mgt       = 0x0000001e
op2hex Comb      = 0x00000018
op2hex Cls       = 0x0000001a
op2hex Arg       = 0x0000001c
op2hex AluImm    = 0x00000013
op2hex Alu       = 0x00000011
op2hex Csr       = 0x00000016
op2hex Fpu       = 0x00000017
op2hex Morph     = 0x00000015
op2hex Xtra      = 0x0000001c


data Funct6 = Type0
            | Type1
            | Type2
            | Type3
            | Type4
            | Type5
            | Type6
            | Type7
            | Type8
            | Type9
            | Type10
            | Type11
            | Type12
            | Type13
            | Type14
            | Type15
            | Type16
            | Type17
            | Type18
            | Type19
            | Type20
            | Type21
            | Type22
            | Type23
            | Type24
            | Type25
            | Type26
            | Type27
            | Type28
            | Type29
            | Type30
            | Type31
            | Type32
            | Type33
            | Type34
            | Type35
            | Type36
            deriving(Show, Eq, Read)
            
f6tohex :: Funct6 -> Integer
f6tohex Type0     = 0x00000000
f6tohex Type1     = 0x00000020
f6tohex Type2     = 0x00000040
f6tohex  Type3     = 0x00000060
f6tohex Type4     = 0x00000080
f6tohex Type5     = 0x000000a0
f6tohex Type6     = 0x000000c0
f6tohex Type7     = 0x000000e0
f6tohex Type8     = 0x00000100
f6tohex Type9     = 0x00000120
f6tohex Type10    = 0x00000140
f6tohex Type11    = 0x00000160
f6tohex Type12    = 0x00000180
f6tohex Type13    = 0x000001a0
f6tohex Type14    = 0x000001c0
f6tohex Type15    = 0x000001e0
f6tohex Type16    = 0x00000200
f6tohex Type17    = 0x00000220
f6tohex Type18    = 0x00000240
f6tohex  Type19    = 0x00000260
f6tohex Type20    = 0x00000280
f6tohex Type21    = 0x000002a0
f6tohex Type22    = 0x000002c0
f6tohex Type23    = 0x000002e0
f6tohex Type24    = 0x00000300
f6tohex Type25    = 0x00000320
f6tohex Type26    = 0x00000340
f6tohex Type27    = 0x00000360
f6tohex Type28    = 0x00000380
f6tohex Type29    = 0x000003a0
f6tohex Type30    = 0x000003c0
f6tohex Type31    = 0x000003e0
f6tohex Type32    = 0x00000400
f6tohex Type33    = 0x00000420
f6tohex Type34    = 0x00000440
f6tohex Type35    = 0x00000460
f6tohex Type36    = 0x00000480
f6tohex _    = error("Combinator type not found")

f2tohex :: Funct2 -> Integer
f2tohex Low  = 0x0
f2tohex High = 0x040000000


func52hex :: Funct5 -> Integer
func52hex F5Add = 0x0
func52hex F5Sub = 0x0040
func52hex F5Sll = 0x0060
func52hex F5Xor = 0x00c0
func52hex F5Srl = 0x00e0
func52hex F5Sra = 0x0100
func52hex F5Or  = 0x0120
func52hex F5And = 0x0140
func52hex F5Mul = 0x0160
func52hex F5Mulh= 0x0180
func52hex F5Mulhsu = 0x01a0
func52hex F5Mulhu  = 0x01c0
func52hex F5Div    = 0x01e0
func52hex F5Divu   = 0x0200
func52hex F5Rem  = 0x0220
func52hex F5Remu = 0x0240
func52hex F5Madd = 0x0260
func52hex F5Msub = 0x0280
func52hex F5Eq = 0x02a0
func52hex F5Gt = 0x02c0
func52hex F5Lt = 0x02e0
func52hex F5Csrr   = 0x0000
func52hex F5Csrrw  = 0x0020
func52hex F5Csrwi  = 0x0040
func52hex F5Csrrwi = 0x0060
func52hex F5Addi = 0x0000
func52hex F5Slli = 0x0060
func52hex F5Xori = 0x00c0
func52hex F5Srli = 0x00e0
func52hex F5Srai = 0x0100
func52hex F5Ori  = 0x0120
func52hex F5Andi = 0x0140
func52hex F5Muli = 0x0160
func52hex F5Mulhi = 0x0180
func52hex F5Divi = 0x01e0
func52hex F5Remi = 0x0220
func52hex F5Maddi = 0x0260
func52hex F5Eqi = 0x02a0
func52hex F5Gti = 0x02c0
func52hex F5Lti = 0x02e0
func52hex F5Fadd = 0x0000
func52hex F5Fsub = 0x0020
func52hex F5Fmul = 0x0060
func52hex F5Fdiv = 0x0080
func52hex F5Fsqrt = 0x00a0
func52hex F5Fti = 0x00c0
func52hex F5Itf = 0x00e0
func52hex F5Fmac = 0x0100
func52hex F5Feq = 0x0120
func52hex F5Flt = 0x0140
func52hex F5Fgt = 0x0160
func52hex F5Cata  = 0x0
func52hex F5Ana   = 0x0020
func52hex F5Hylo  = 0x0040
func52hex F5Para  = 0x0060
func52hex F5Apo   = 0x0080
func52hex F5Histo = 0x00a0
func52hex F5Out = 0x0200
func52hex F5Fork = 0x0000
func52hex F5Lam = 0x0040
func52hex F5Fix = 0x0020
func52hex F5Par = 0x0060
func52hex F5PushArg1 = 0x0080
func52hex F5PushArg2 = 0x00a0
func52hex F5PushArg3 = 0x00c0
func52hex F5PushArg4 = 0x00e0
func52hex F5ForceEval = 0x0100
func52hex F5PushEval = 0x0120
func52hex F5Break    = 0x0140
func52hex F5Root = 0x003e0
func52hex  _ = error("funct5 not found")













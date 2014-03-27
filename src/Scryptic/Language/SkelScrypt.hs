module Scryptic.Language.SkelScrypt where

-- Haskell module generated by the BNF converter

import Scryptic.Language.AbsScrypt
import Scryptic.Language.ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x


transScrypt :: Scrypt -> Result
transScrypt x = case x of
  OneBlock block  -> failure x
  MultiBlock blocks  -> failure x


transBlock :: Block -> Result
transBlock x = case x of
  Block blockopts stmts  -> failure x


transBlockOpt :: BlockOpt -> Result
transBlockOpt x = case x of
  TitleOpt id  -> failure x


transStmt :: Stmt -> Result
transStmt x = case x of
  Wait expr  -> failure x
  Write skey soptval  -> failure x
  WriteSync skey soptval expr  -> failure x
  Watch skey  -> failure x
  Unwatch skey  -> failure x
  Sleep snum  -> failure x
  SetOpt skey id  -> failure x


transExpr :: Expr -> Result
transExpr x = case x of
  OrExpr expr1 expr2  -> failure x
  AndExpr expr1 expr2  -> failure x
  CmpExpr expr1 cmpop2 expr3  -> failure x
  RCmpExpr sval1 cmpop2 skey3 cmpop4 sval5  -> failure x
  RConstExpr sval  -> failure x
  KeyExpr skey  -> failure x


transSKey :: SKey -> Result
transSKey x = case x of
  SKey namequals id  -> failure x


transNameQual :: NameQual -> Result
transNameQual x = case x of
  NameQual id  -> failure x


transSOptVal :: SOptVal -> Result
transSOptVal x = case x of
  SOptVal sval  -> failure x
  SOptNone  -> failure x


transSVal :: SVal -> Result
transSVal x = case x of
  SValNum snum  -> failure x
  SValStr str  -> failure x


transSNum :: SNum -> Result
transSNum x = case x of
  IntNum n  -> failure x
  DubNum d  -> failure x


transCmpOp :: CmpOp -> Result
transCmpOp x = case x of
  EqOp  -> failure x
  NEqOp  -> failure x
  LtOp  -> failure x
  GtOp  -> failure x
  LEqOp  -> failure x
  GEqOp  -> failure x




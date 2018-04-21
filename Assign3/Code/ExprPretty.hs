module ExprPretty where

import           ExprType

parens :: String -> String
parens ss = "(" ++ ss ++ ")"

instance Show a => Show (Expr a) where
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)
  show (Add e1 e2)  = parens (show e1) ++ " !+ " ++ parens (show e2)
  show (Var ss)     = parens $ "var \"" ++ ss ++ "\""
  show (Const x)    = parens $ "val " ++ show x
  show (Ln x) = parens $ "Ln " ++ show x
  show (Exp x) = parens $ "e ^ " ++ show x
  show (Cos x) = parens $ "Cos " ++ show x
  show (Sin x) = parens $ "Sin " ++ show x
  show (Exponent e1 e2) = parens $ show e1 ++ " ^ " ++ show e2

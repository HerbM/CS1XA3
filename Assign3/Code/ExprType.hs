module ExprType where

import           Data.List



-- | Datatype for regularly used expressions
-- data Expr a = Add (Expr a) (Expr a) -- ^ simple addition with 2 expr types
--             | Mult (Expr a) (Expr a) -- ^ simple Mulitplication with 2 expr types
--                         | Const a -- ^ to wrap a constant
--                                     | Var String -- ^ identifies a variable
--                                                 | Cos (Expr a) -- ^ Constructor for cos
--                                                             | Sin (Expr a) -- ^ Constructor 
--                                                                         | Ln (Expr a) -- ^ natural log
--                                                                                     | Exp (Expr a) -- ^ e (to the power of) (Expr)
--                                                                                                 | Exponent (Expr a) (Expr a)
--                                                                                                   deriving Eq
--
--
--                                                                                                   {- | getVars Takes the variables from an expression recursively
--                                                                                                   	 if the expression is in Var constructor form it returns the variable, otherwise it strips it down
--                                                                                                   	 	 	until it is in Var form
--                                                                                                   	 	 		 if the expression is a constant, there is no variable, so it returns nothing.
--                                                                                                   	 	 		 -}
--                                                                                                   	 	 		 getVars :: Expr a -> [String]
--                                                                                                   	 	 		 getVars (Add e1 e2)  = getVars e1 `union` getVars e2
--                                                                                                   	 	 		 getVars (Mult e1 e2) = getVars e1 `union` getVars e2
--                                                                                                   	 	 		 getVars (Const _)    = []
--                                                                                                   	 	 		 getVars (Var ident)  = [ident]
--                                                                                                   	 	 		 getVars (Cos e1)     = getVars e1
--                                                                                                   	 	 		 getVars (Sin e1)     = getVars e1
--                                                                                                   	 	 		 getVars (Exp e1)     = getVars e1
--                                                                                                   	 	 		 getVars (Ln e1)      = getVars e1
--

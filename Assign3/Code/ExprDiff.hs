{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module ExprDiff where

import           ExprType

import qualified Data.Map.Strict as Map

{-|
 - Module : ExprDiff
 - Description : Contains Basic Mathematical functions including: 
 - 				partial differentiation, simplification and evaluation
 - 				Copyright : (c) Hariesh Jayanthan @2018 
 - 				License : WTFPL
 - 				Maintainer : Jayanthh@mcmaster.ca
 - 				Stability : experimental
 - 				Portability : DOS
 -
 - 				-}

{- Class DiffExpr:
 - 		Differentiable Expressions
 - 		 - This Class has methods over the Expr datatype
 - 		    that assist in with construction and evaluation
 - 		       of Differentiable expressions.
 -
 - 		        - Methods:
 - 		         - eval: takes a dictionary of variable identifiers and values, and uses it to compute the Expr Fully
 - 		          - Simplify: takes a possibly incomplete dictionary and uses it to reduce Expr as much as possible
 - 		           		ie e1 = x + y, e2 = y + x, simplify e1 == simplify e2
 - 		           		 		ie Add (Add (Var "x") (Const 1)) (Add (Const 2) (Var "y"))
 - 		           		 		 		   => Add (Const 3) (Add (Var "x") (Var "y"))
 - 		           		 		 		    - partDiff: given a var identifier, differenetiate in terms of that identifier
 - 		           		 		 		     - Default Methods:
 - 		           		 		 		      		!+, !*, val, var : are function wrappers for Expr constructors that perform additional simplification
 - 		           		 		 		      		-}


class DiffExpr a where
  eval :: Map.Map String a -> Expr a -> a
  simplify :: Map.Map String a -> Expr a -> Expr a
  partDiff :: String -> Expr a -> Expr a
  multDiff :: String -> Int -> Expr a -> Expr a

  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2
  val :: a -> Expr a
  val x = Const x
  var :: String -> Expr a
  var x = Var x
  (!^) :: Expr a -> Expr a -> Expr a
  b !^ x = simplify (Map.fromList []) (Exponent b x)
  nCos :: Expr a -> Expr a
  nCos e1 = Cos e1
  nSin :: Expr a -> Expr a
  nSin e1 = Sin e1
  nExp :: Expr a -> Expr a
  nExp e1 = Exp e1
  nLn :: Expr a -> Expr a
  nLn e1 = Ln e1


instance (Floating a, Eq a) => DiffExpr a where
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Const x) = x
  eval vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> v
                       Nothing -> error "failed lookup in eval"
  eval vrs (Exp e) = exp (eval vrs e)
  eval vrs (Exponent b x) = (eval vrs b) ** (eval vrs x)
  eval vrs (Cos e) = cos (eval vrs e)
  eval vrs (Sin e) = sin (eval vrs e)
  eval vrs (Ln e) = log (eval vrs e)


  {- | using simplify for addition
 -   		Functionality: - adding 0 anything doesnt change the expression
 -   						   - Adding two constants returns the Const wrapper around their sum 
 -   						   				   - Anything else returns Add of the two arguments
 -   						   				   	-}

  simplify vrs (Add e1 e2) 	  = let
                                 arg1 = simplify vrs e1
                                 arg2 = simplify vrs e2
                              in case (arg1,arg2) of 
                                   			(Const (0), arg2)     -> arg2
                                   			(arg1,Const 0)       -> arg1                               	   
                                   			(Const a,Const b)    -> Const (a+b)
                                   			(arg1,arg2)          -> Add arg1 arg2

 

  {- | using simplify for Mulitplication
 -   		Functionality: - multiplying by 1 to anything doesnt change the expression
 -   		  				   - multiplying by 0 returns Const 0
 -   		  				   				   - multiplying two constants returns the Const wrapper around their Product
 -   		  				   				   	-}

  simplify vrs (Mult e1 e2)  = let
                                arg1 = simplify vrs e1
                                arg2 = simplify vrs e2 
                            in case (arg1,arg2) of
                                 (Const (0),_)       -> Const (0)
                                 (_,Const (0))       -> Const (0)
                                 (Const 1,arg2)    -> arg2
                                 (arg1,Const 1)    -> arg1
                                 (Const a,Const b) -> Const (a*b)
                                 (arg1,arg2)       -> Mult arg1 arg2


  {- | using simplify for Exp, Exponent, Natural Log
 -   		Functionality: 
 -   						   - ln (1) return 0 
 -   						   				   - ln, exp, and exponent of a constant can be evaluated
 -   						   				   				   - anyvalue raised to 0 returns 1 (this does not consider 0 as a base)
 -   						   				   				   	-}

  simplify vrs (Exponent e1 e2)  = let
                                  arg1 = simplify vrs e1
                                  arg2 = simplify vrs e2
                                        in case (arg1,arg2) of
                                           (Const a,Const b) -> Const ( eval vrs (Exponent arg1 arg2))
                                           (arg1,arg2)         -> Exponent arg1 arg2

  simplify vrs (Ln e1)  = let 
                          arg1 = simplify vrs e1
                                in case arg1 of
                                     (Const 1)         -> Const 0
                                     (Const a)         -> Const $ eval vrs (Ln arg1)
                                     (elsee)             -> Ln elsee

  simplify vrs (Exp e1) = let
                            arg1 = simplify vrs e1
                                 in case arg1 of
                                      (Const 0)         -> Const 1
                                      (Const a)         -> Const $ eval vrs (Exp arg1)
                                      (arg)             -> Exp arg

  {- | using simplify on cos and sin
 -   		Functionality: - if given a constant, evaluates
 -   		  				  - Else returns wrapped in cos or sin -}

  simplify vrs (Cos e1) = let
                            arg1 = simplify vrs e1
                               in case arg1 of
                                (Const a)   -> Const $ eval vrs (Cos e1)
                                arg         -> Cos arg
  

  simplify vrs (Sin e1)  = let
                            arg1 = simplify vrs e1
                               in case arg1 of
                                (Const a)   -> Const $ eval vrs (Sin e1)
                                arg         -> Sin arg

  simplify arg (Var e) = case Map.lookup e arg of
                        Just v -> (Const v)
                        Nothing -> (Var e)
  {- | PartDiff Functionality: 
 -   				  - Derivative of a constant returns 0
 -   				    				  - Else returns wrapped in cos or sin
 -   				    				    				  - Derivative of two expressions added
 -   				    				    				    				    together is the derivative of each respective part added together
 -   				    				    				    				      				  - Derivative of two expressions added uses the chain rule
 -   				    				    				    				      				    				  - Derivative of sin is cos, derivative of cos is - sin
 -   				    				    				    				      				    				    				  - Derivative of an exponent is done using the equation of the derivative of a function
 -   				    				    				    				      				    				    				    				  - If a variable appears that is the variable being differentiated for, it becomes 1, else it becomes 0
 -
 -
 -   				    				    				    				      				    				    				    				    				     -}

  partDiff vrs (Mult arg1 arg2) = Add (Mult arg1 (partDiff vrs arg2)) (Mult arg2 (partDiff vrs arg1))
  partDiff vrs (Add arg1 arg2) = Add (partDiff vrs arg1) (partDiff vrs arg2)
  partDiff vrs (Cos arg1) = Mult (Mult (Const (-1)) (Sin arg1)) ( partDiff vrs arg1)
  partDiff vrs (Sin arg1) = Mult (Cos arg1) ( partDiff vrs arg1 )
  partDiff vrs (Ln arg1)   = Mult (partDiff vrs arg1) ( Exponent arg1  ( Const ( -1)))
  partDiff ss (Exponent e1 e2) = Add (Mult (Exponent e1 e2) $ Mult (partDiff ss e2) $ Ln e1) $ Mult (Exponent e1 $ Add e2 $ Const $ -1) $ Mult e2 $ partDiff ss e1
  partDiff ss (Exp e1) = Mult (Exp e1) $ partDiff ss e1
  partDiff vrs (Const _) = Const 0
  partDiff vrs (Var y) = if y == vrs 
  							then (Const 1) 
  								else (Const 0)


 -- multDiff vrs expression degree = iterate (partDiff (vrs) (expression)) (degree) !! degree
 --

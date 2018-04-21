module ExprTest where

import           ExprDiff
import           ExprParser
import           ExprPretty
import           ExprType

import qualified Data.Map.Strict as Map
import           Test.QuickCheck


evalFcheck :: Float -> Float -> Float -- ^ Quick Check produces 3 random Floats that are inputted into the equation, which is then compared against the normal mathematical equation to see if they produce the same answer to some tolerance
                                 -> Bool -- ^ If they are similar enough, True is returned, else False is returned.
evalFcheck arg1 arg2 arg3 = 
                 let x = (eval (Map.fromList[("i", 1.0)]) (Add (Mult (Add (Const arg1) (Const arg2)) (Var "i")) (Mult (Const arg3) (Var "i")))) 
                 in abs (x - (((arg1+arg2)) + (arg3))) <= 1.0 

evalDcheck :: Double -> Double -> Double -- ^ Quick Check produces 3 random Double Values that are inputted into the equation, which is then compared against the normal mathematical equation to see if they produce the same answer to some tolerance
             -> Bool -- ^ True is returned if the answers are within tolerance of each other
evalDcheck arg1 arg2 arg3 = 
                 let x = (eval (Map.fromList[("i", 1.0)]) (Add (Mult (Add (Const arg1) (Const arg2)) (Var "i")) (Mult (Const arg3) (Var "i")))) 
                 in abs (x - (((arg1+arg2)) + (arg3))) <= 1.0 


dxDouble :: Double -- ^ this time a double variable value is being generated with QuickCheck rather than constants
           -> Bool -- ^ If it is within tolerance the function responds with True
dxDouble arg1 = 
    let x = eval (Map.fromList[("x", arg1)]) ((partDiff "x" (Add (Mult (Exp (Const 1.0)) (Const 2.0)) (Mult (Const 4.0) (Var "x")))))
    in abs((x) - 4.0) <= 1.0

dxFloat :: Float -- ^ a float variable value is being generated with QuickCheck rather than constants
           -> Bool -- ^ If it is within tolerance the function responds with True
dxFloat arg1 = 
    let x = eval (Map.fromList[("x", arg1)]) ((partDiff "x" (Add (Mult (Exp (Const 1.0)) (Const 2.0)) (Mult (Const 4.0) (Var "x")))))
    in abs((x) - 4.0) <= 1.0



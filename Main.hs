{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, 
    FlexibleInstances #-}

module Main where

import WhilePP

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer

import Test.HUnit hiding (State)

type Store    = Map Variable Value
evalS :: (MonadState Store m, MonadError Value m, MonadWriter String m) => Statement -> m ()
evalS = undefined
 
-- | 
-- Updated Store s'
-- Error: Just Value or Nothing
-- String: Log
-- 
evalE :: Store -> Expression -> (Store, Maybe Value, String) 
evalE = error "evalE"
 

execute :: Store -> Statement -> (Store, Maybe Value, String)
execute = undefined

raises :: Statement -> Value -> Test
s `raises` v = case (execute Map.empty s) of
    (_, Just v', _) -> v ~?= v'
    _  -> undefined

t1 :: Test
t1 = (Assign "X"  (Var "Y")) `raises` IntVal 0

t2 :: Test
t2 = (Assign "X" (Op Divide (Val (IntVal 1)) (Val (IntVal 0)))) `raises` IntVal 1

t3 :: Test       
t3 = TestList [ Assign "X" (Op Plus (Val (IntVal 1)) (Val (BoolVal True))) `raises` IntVal 2,      
                If (Val (IntVal 1)) Skip Skip `raises` IntVal 2,
                While (Val (IntVal 1)) Skip `raises` IntVal 2]

mksequence :: [Statement] -> Statement
mksequence = foldr Sequence Skip

testprog1 :: Statement
testprog1 = mksequence [Assign "X" $ Val $ IntVal 0,
                        Assign "Y" $ Val $ IntVal 1,
                        Print "hello world: " $ Var "X",
                        If (Op Lt (Var "X") (Var "Y")) (Throw (Op Plus (Var "X") (Var "Y")))
                                                       Skip,
                        Assign "Z" $ Val $ IntVal 3]

t4 :: Test
t4 = execute Map.empty testprog1 ~?=
  (Map.fromList [("X", IntVal 0), ("Y",  IntVal 1)], Just (IntVal 1), "hello world: 0")

testprog2 :: Statement
testprog2 = mksequence [Assign "X" $ Val $ IntVal 0,
                        Assign "Y" $ Val $ IntVal 1,
                        Try (If (Op Lt (Var "X") (Var "Y"))
                                (mksequence [Assign "A" $ Val $ IntVal 100,
                                             Throw (Op Plus (Var "X") (Var "Y")),
                                             Assign "B" $ Val $ IntVal 200])
                                Skip)
                            "E"
                            (Assign "Z" $ Op Plus (Var "E") (Var "A"))]

t5 :: Test
t5 = execute Map.empty testprog2 ~?=
   ( Map.fromList [("A", IntVal 100), ("E", IntVal 1)
          ,("X", IntVal 0), ("Y", IntVal 1)
          ,("Z", IntVal 101)]
          , Nothing 
   , "")

main :: IO ()
main = do 
   _ <- runTestTT $ TestList [ t1, t2, t3, t4, t5 ]
   return ()


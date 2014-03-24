-- Advanced Programming, HW 5
-- by Daniel Cabrera, dcabrera
--    Hangfei Lin, hangfei


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

errorTypeZero :: Value
errorTypeZero = IntVal 0

errorTypeOne :: Value
errorTypeOne = IntVal 1

errorTypeTwo :: Value
errorTypeTwo = IntVal 2

-- | Evaluate an statement and return ()
evalS :: (MonadState Store m, MonadError Value m, MonadWriter String m) => Statement -> m ()
evalS (Print str e)    = do v <- runErrorT $ evalE e
                            case v of
                              Left _   -> tell $ str
                              Right r  -> tell $ str ++ display r
evalS Skip             = return ()
evalS (Assign v e)     = do x <- evalE e
                            m <- get
                            put $ Map.insert v x m
evalS (Throw e)        = evalE e >>= (\s -> throwError s)
evalS (Sequence s1 s2) = do evalS s1
                            evalS s2
                            return ()
evalS (If e s1 s2)     = do b <- evalE e
                            case b of 
                              BoolVal True  -> evalS s1
                              BoolVal False -> evalS s2
                              _             -> throwError errorTypeTwo 
evalS (While e s)      = do b <- evalE e
                            case b of
                              BoolVal True -> do evalS s
                                                 evalS $ While e s
                              BoolVal False -> return ()
                              _             -> throwError errorTypeTwo
evalS (Try s x h)      = do i <- runErrorT $ evalS s
                            case i of
                              Right _      -> return ()
                              Left l       -> do _ <- evalS (Assign x (Val l))
                                                 b <- (evalS h) 
                                                 return b

-- | Evaluate an expression and return a Value with Monad
evalE :: (MonadState Store m, MonadError Value m) => Expression -> m Value
evalE (Var x)        = do s <- get
                          case (Map.lookup x s) of
                              Just i -> return i
                              Nothing -> throwError errorTypeZero 
evalE (Val v)        = return v
evalE (Op bop e1 e2) = do s1 <- evalE e1
                          s2 <- evalE e2
                          evalBop bop s1 s2 where
                              evalBop Plus   (IntVal v1) (IntVal v2) 
                                = return $ IntVal (v1 + v2)
                              evalBop Times  (IntVal v1) (IntVal v2) 
                                = return $ IntVal (v1 * v2)
                              evalBop Minus  (IntVal v1) (IntVal v2) 
                                = return $ IntVal (v1 - v2)
                              evalBop Divide (IntVal _) (IntVal 0)  
                                = throwError errorTypeOne
                              evalBop Divide (IntVal v1) (IntVal v2) 
                                = return $ IntVal (v1 `div` v2)
                              evalBop Gt     (IntVal v1) (IntVal v2) 
                                = return $ BoolVal (v1 > v2)
                              evalBop Ge     (IntVal v1) (IntVal v2) 
                                = return $ BoolVal (v1 >= v2)
                              evalBop Lt     (IntVal v1) (IntVal v2) 
                                = return $ BoolVal (v1 < v2)
                              evalBop Le     (IntVal v1) (IntVal v2)  
                                = return $ BoolVal (v1 <= v2)
                              evalBop _      _           _           
                                = throwError errorTypeTwo


-- | Evaluate the statment with actual multi-monadic features
evalES :: Statement -> ErrorT Value (WriterT String (State Store)) ()
evalES = evalS

-- | Execute the evaluation, and returns a triple
execute :: Store -> Statement -> (Store, Maybe Value, String)
execute s stat = let ((m, str), st) = runState (runWriterT $ runErrorT (evalES stat)) s
                 in case m of
                     Left v  -> (st, Just v, str)
                     Right _ -> (st, Nothing, str)


---------------------
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


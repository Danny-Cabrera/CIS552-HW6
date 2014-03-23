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


defaultVal :: Value
defaultVal = IntVal 0
evalBop :: Bop -> Value -> Value -> Value
evalBop Plus   (IntVal v1) (IntVal v2) = IntVal (v1 + v2)
evalBop Times  (IntVal v1) (IntVal v2) = IntVal (v1 * v2)
evalBop Minus  (IntVal v1) (IntVal v2) = IntVal (v1 - v2)
evalBop Divide (IntVal v1) (IntVal v2) = IntVal (v1 `div` v2)
evalBop Gt     (IntVal v1) (IntVal v2) = BoolVal (v1 > v2)
evalBop Ge     (IntVal v1) (IntVal v2) = BoolVal (v1 >= v2)
evalBop Lt     (IntVal v1) (IntVal v2) = BoolVal (v1 < v2)
evalBop Le     (IntVal v1) (IntVal v2) = BoolVal (v1 <= v2)
evalBop _      _           _           = defaultVal


type Store    = Map Variable Value
-- tell str: log in the MonadWriter, update the state?
-- it's polymorphic?
evalS :: (MonadState Store m, MonadError Value m, MonadWriter String m) => Statement -> m ()
evalS (Print str _) = do tell str
                         return ()


-- evaluate expresion e, see if it throws exception. 
-- | Throw the exception carrying the value evaluted from expression e
-- what if it doesn't throw exception?
evalS (Throw e) = evalE e >>= (\s -> throwError s)

-- 
-- Try Statement Variable Statement
-- Try s x h should execute the statement s and if, 
-- in the course of execution, an exception is thrown, 
-- then the exception value should be assigned to the variable 
-- x after which the handler statement h is executed.
-- 
-- Check (evalS s): is there exception
-- if yes, assign 
-- evalS (Try s x h) = do i <- evalS s
--                       if exception then do add i
--                                            evalS h
--                                    else return ()
-- how do i track the exception???



evalS (Try s x h) = do i <- runErrorT $ evalS s
                       case i of
                         Left l       -> (evalS h)
                         Right r      -> return ()


--                     Assign x $ Val i
--                       return ()
-- evalS _ = error "evals Not Implemented"
 


-- evaluate expression?
evalE :: (MonadState Store m, MonadError Value m, MonadWriter String m) => Expression -> m Value
-- return?
eavlE (Var x)        = do s <- get
                          return $ s Map.! x
-- return?
evalE (Op bop e1 e2) = do s1 <- evalE e1
                          s2 <- evalE e2
                          return $ evalBop bop s1 s2
-- Multiple declarations of `evalE'
evalE (Val v)        = return v  

-- evalE (Var x)        = do s <- get
--                           return $ s Map.! x
-- evalE (Op bop e1 e2) = do s1 <- evalE e1
--                          s2 <- evalE e2
--                           return $ evalBop bop s1 s2

-- 
-- evalS stat :: m ()
-- WriterT String (StateT Int (Either String)) a
evalES :: Statement -> WriterT String (StateT Store (Either Value)) ()
evalES = evalS

execute :: Store -> Statement -> (Store, Maybe Value, String)
execute s stat = let p1 = (runStateT $ runWriterT (evalES stat)) s
                 in case p1 of
                      Left v  -> (Map.empty, Just v, "Test")
                      Right p -> (snd p, Just (IntVal 3), snd $ fst p)

-- pp1 :: Store -> Statement -> Either Value (((), String), Store)
-- pp1 s stat = (runStateT $ runWriterT (evalES stat)) s

-- pp2 :: Store -> Statement -> StateT Store (Either Value) ((), String)
-- pp2 s stat = runWriterT (evalES stat)


-- pp3 :: Store -> Statement -> WriterT String (StateT Store (Either Value)) ()
-- pp3 s stat =  (evalES stat)


-- (Map.empty, Just (IntVal 3), "Test")
-- let r1 =  do x <- (runStateT (evalS stat)) s
 --                             x
  --               in (snd r1, Just (IntVal 3), "Test")

--                  in (r1, Just (IntVal 3), "Test")
-- execute = undefiend

-- ???


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


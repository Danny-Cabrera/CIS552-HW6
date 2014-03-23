-- Advanced Programming, HW 5
-- by Daniel Cabrera, dcabrera
--    Hangfei Lin, hangfei


{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
   
module MonadTransformers where
 
import Prelude hiding (Maybe, Just, Nothing)
import Control.Monad (liftM)
import Test.QuickCheck
import Test.QuickCheck.Function

import Control.Monad.Error  (ErrorT)
import Control.Monad.Reader (Reader)
import Control.Monad.List   (ListT)

import FunctorMonadLaws

data Maybe a = Just a
             | Nothing
             deriving (Show, Eq)
 
instance Functor Maybe where
  fmap f (Just x) = Just $ f x
  fmap _ Nothing  = Nothing
 
instance Monad Maybe where
  return        = Just
  Nothing >>= _ = Nothing
  Just x  >>= f = f x

instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = frequency [ (1, return Nothing)
                        , (4, liftM Just arbitrary)
                        ]
  shrink Nothing  = []
  shrink (Just x) = Nothing : map Just (shrink x)

qc_Maybe :: IO ()
qc_Maybe = quickCheck (prop_Monad :: Char -> Maybe Char ->
                                     Fun Char (Maybe Int) ->
                                     Fun Int (Maybe String) -> Bool)

newtype MaybeT m a = MT { unMT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
  return x = MT $ return $ Just x
  MT x >>= f = MT $ do ea <- x  
                       case ea of 
                         Nothing -> return Nothing
                         Just a  -> unMT $ f a


instance Eq a => Eq (MaybeT [] a) where
  MT x == MT y = x == y

instance Show a => Show (MaybeT [] a) where
  show (MT x) = show x

instance Arbitrary a => Arbitrary (MaybeT [] a) where
  arbitrary = liftM MT arbitrary
  shrink    = map MT . shrink . unMT

qc_MaybeT :: IO ()
qc_MaybeT = quickCheck (prop_Monad :: Char -> MaybeT [] Char ->
                                         Fun Char (MaybeT [] Int) ->
                                         Fun Int (MaybeT [] String) -> Bool)


-- | 
data MaybeW m a = MW { unMaybeW :: Maybe (m a) }

instance Monad m => Monad (MaybeW m) where

  return x = MW $ Just $ return x

  MW Nothing  >>= f = MW Nothing
  -- (>>=) :: Maybe (m a) -> (a -> Maybe (m b)) -> Maybe (m b)
  -- MW (Just x) >>= f = ??
  MW (Just _x) >>= f = MW Nothing


instance Eq a => Eq (MaybeW [] a) where
  MW x == MW y = x == y

instance Show a => Show (MaybeW [] a) where
  show (MW x) = show x

instance Arbitrary a => Arbitrary (MaybeW [] a) where
  arbitrary = liftM MW arbitrary
  shrink    = map MW . shrink . unMaybeW

qc_MaybeW :: IO ()
qc_MaybeW = quickCheck (prop_Monad :: Char -> MaybeW [] Char ->
                                      Fun Char (MaybeW [] Int) ->
                                      Fun Int (MaybeW [] String) -> Bool)


-- Part3
class (Monad m, Monad (t m)) => MonadTrans t m where
  lift :: Monad m => m a -> t m a

instance Monad m => MonadTrans MaybeT m where
  lift = MT . lift_ where
    lift_ mt = do x <- mt
                  return $ Just x  
              
-- | prop_MonadTransReturn
-- Lifting a "null" computation in m yields a "null" computation in t m
prop_MonadTransReturn :: Int -> Bool
prop_MonadTransReturn x = (lift . return) x == ((return x) :: MaybeT [] Int)

-- | prop_MonadTransBind
-- Lifting a sequence of computations is the same as lifting them individually
prop_MonadTransBind :: [Char] -> Fun Char [Char] -> Bool
prop_MonadTransBind x (Fun _ f) = ((lift (x >>= f)) ::  MaybeT [] Char) == ((lift x >>= (lift .f)) ::  MaybeT [] Char)

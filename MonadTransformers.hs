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
  -- x :: m (Maybe a)
  -- ea :: (Maybe a)
  -- f :: a -> MaybeT m a
  -- f a :: MaybeT m a
  -- unMT $ f a :: m (Maybe a) 
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
-- qc_MaybeT = error "Implement qc_MaybeT"

-- qc_Maybe = quickCheck (prop_Monad :: Char -> Maybe Char ->
--                                     Fun Char (Maybe Int) ->
--                                     Fun Int (Maybe String) -> Bool)


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
-- |
-- Lifting a "null" computation in m yields a "null" computation in t m
-- where is the null?
prop_MonadTransReturn :: Int -> Bool
-- prop_MonadTransReturn x = (lift . return) x == (return :: a -> MaybeT [] a) x
prop_MonadTransReturn x = (lift . return) x == ((return x) :: MaybeT [] Int)

--prop_MonadTransReturn x = x == return x

{-
prop_MonadTransReturn' x = (lift . return_m) x == return_tm x
	where return_m :: Monad m => a -> m a
        -- return_m :: a -> [a]
        return_m = return
         
        -- return_tm :: a 
        -- return_tm :: a -> MaybeT m a
        return_tm = return
-}

-- lift ( m a)
-- (return x) == lift x
-- lift (m >>=_m k) = lift m >>=_tm (lift . k)
-- m :: Monad
-- k :: a -> m b
-- m >>= k :: m a -> (a -> m b) -> m b
-- lift (m >>= k) :: t m b

-- lift m :: t m a
-- lift .k a == lift ( k a)
-- :: lift (m b) :: t m b
-- m >>= k :: 
-- lift m :: t m a
-- lift .k a :: t m a
prop_MonadTransBind :: [Char] -> (Char -> [Int]) -> Bool
prop_MonadTransBind x k = ((lift (x >>= k)) ::  MaybeT [] Int) == ((lift x >>= (lift .k)) ::  MaybeT [] Int)
-- prop_MonadTransBind = undefined


{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module ParserTrans (GenParser, Parser, 
                   getC,
                   getElement,
                   choose,
                   (<|>),
                   satisfy,
                   doParse,  
                   eof
                   ) where

-- import Control.Monad.List
import Control.Monad.State

-- newtype GenParser e a = P ([e] -> [(a, [e])])
-- runState :: s -> (a, s)
-- 
newtype GenParser e a = P (StateT [e] [] a)
type Parser a = GenParser Char a
-- StateT ([e] -> [(a, [e])]): (StateT [e] [] a)
-- P (StateT [e] [] a): GenParser e a
--   P is the constructor for GenParser
-- runStateT: deconstructor, (StateT [e] [] a) => s -> m (a, s)

-- type Parser a = StateT String [] a
--     ==> StateT (String -> [(a,String)])
-- s - The state.
     -- String
-- m - The inner monad.
     -- monad []

-- ???
-- StateT	 
-- runStateT :: s -> m (a, s)
-- runState (StateT [e] [] a) ===> \s -> m (a, s)
-- deconstructor?
doParse :: GenParser e a  -> [e] -> [(a,[e])]
doParse (P p) s = (runStateT p) s

{-
get :: Parser Char
get = P (\cs -> case cs of 
                (x:xs) -> [ (x,xs) ]
                []     -> [])
-}

-- get: ??
-- get for normal state: get (Store, Value) would get Store(current State)
-- put xs: updates the state
-- return x: return the value
-- | Return the next character
getC :: GenParser e e 
getC = P $ do (x : xs) <- get
              put xs
              return x
              
getElement :: GenParser e e
getElement = P $ StateT (\es -> case es of
                                  (x:xs) -> [(x, xs)]
                                  []     -> []) 

-- | Return the next character if it satisfies the given predicate
-- (this was called satP in lecture)
satisfy :: (e -> Bool) -> GenParser e e
satisfy p = do x <- getC
               if (p x) then return x
                        else fail "satisfy: not satisfied."

-- GenParser e e is not a Monad
-- what's the type of x
-- ?????
-- newtype GenericParser s a = GP { unGP :: StateT [s] [] a }
-- runStateT ::
-- f :: (a -> m b)
-- GenParser e a

instance Monad (GenParser e) where
-- ???
    return x    =  P $ return x
    (P p) >>= f = P $ (StateT (\cs -> do (a,cs') <- (runStateT p) cs 
                                         doParse (f a) cs')) 
    fail _     = P $ StateT (\_ ->  [ ] ) 
--  return x = return x

{-
instance Monad Parser where
   p1 >>= fp2 = P (\cs -> do (a,cs') <- doParse p1 cs 
                             doParse (fp2 a) cs') 

   return x   = P (\cs -> [ (x, cs) ])

   fail _     = P (\_ ->  [ ] )
instance Monad (GenericParser e) where
  -- p1 >>= fp2 = GP $ StateT (\cs -> do (a, cs') <- doParse p1 cs
  --                                    doParse (fp2 a) cs')
  (GP x) >>= f = GP $ x >>= (unGP . f)

  return x = GP $ return x

  --return x   = GP $ StateT (\cs -> [ (x, cs) ])
  fail _     = GP $ StateT (\_ ->  [ ] )
-}

--instance Monad (GenericParser e) where
--   p1 >>= fp2 = P (\cs -> do (a,cs') <- doParse p1 cs 
--                            doParse (fp2 a) cs') 

--   return x   = P (\cs -> [ (x, cs) ])

--   fail _     = P (\_ ->  [ ] )


-- choose :: GenericParser e a -> GenericParser e a -> GenericParser e a
-- p1 `choose` p2 = P (\cs -> doParse p1 cs ++ doParse p2 cs)
-- | Combine two parsers together in parallel, producing all 
-- possible results from either parser.                 
choose :: GenParser e a -> GenParser e a -> GenParser e a
p1 `choose` p2 = P $ StateT (\cs -> doParse p1 cs ++ doParse p2 cs)


-- (<|>) :: GenericParser e a -> GenericParser e a -> GenericParser e a
-- p1 <|> p2 = P $ \cs -> case doParse (p1 `choose` p2) cs of
--                          []   -> []
--                          x:_ -> [x]
-- | Combine two parsers together in parallel, but only use the 
-- first result. This means that the second parser is used only 
-- if the first parser completely fails. 
(<|>) :: GenParser e a -> GenParser e a -> GenParser e a
p1 <|> p2 = P $ StateT (\cs -> case doParse (p1 `choose` p2) cs of
                                 []    -> []
                                 x : _ -> [x])
-- | eof
-- End of File
-- type Parser a = GenParser Char a
-- Parser () = GenParser Char ()
-- newtype GenParser e a = P ([e] -> [(a, [e])])
-- newtype GenParser Char () = P (String -> [((), String)])
-- ?
eof :: Parser ()
eof = P $ StateT (\cs -> case cs of
                           []  -> [((),[])]
                           _:_ -> [])

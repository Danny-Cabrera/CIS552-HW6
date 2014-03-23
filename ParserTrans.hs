-- Advanced Programming, HW 5
-- by Daniel Cabrera, dcabrera
--    Hangfei Lin, hangfei

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

import Control.Monad.State

newtype GenParser e a = P (StateT [e] [] a)
type Parser a = GenParser Char a

-- | doParse
-- do the parse
doParse :: GenParser e a  -> [e] -> [(a,[e])]
doParse (P p) s = (runStateT p) s

-- | getC
-- get next character
getC :: GenParser e e 
getC = P $ do (x : xs) <- get
              put xs
              return x
-- | getElement
-- get next element              
getElement :: GenParser e e
getElement = P $ StateT (\es -> case es of
                                  (x:xs) -> [(x, xs)]
                                  []     -> []) 

-- | Return the next character if it satisfies the given predicate
-- (this was called satP in lecture)
satisfy :: (e -> Bool) -> GenParser e e
satisfy p = do x <- getC
               if (p x) then return x
                        else fail "fail satisfy: not satisfied."


instance Monad (GenParser e) where
    return x    =  P $ return x
    (P p) >>= f = P $ (StateT (\cs -> do (a,cs') <- (runStateT p) cs 
                                         doParse (f a) cs')) 
    fail _     = P $ StateT (\_ ->  [ ] ) 

-- | Combine two parsers together in parallel, producing all 
-- possible results from either parser.                 
choose :: GenParser e a -> GenParser e a -> GenParser e a
p1 `choose` p2 = P $ StateT (\cs -> doParse p1 cs ++ doParse p2 cs)

-- | Combine two parsers together in parallel, but only use the 
-- first result. This means that the second parser is used only 
-- if the first parser completely fails. 
(<|>) :: GenParser e a -> GenParser e a -> GenParser e a
p1 <|> p2 = P $ StateT (\cs -> case doParse (p1 `choose` p2) cs of
                                 []    -> []
                                 x : _ -> [x])

-- | eof
-- End of File
eof :: Parser ()
eof = P $ StateT (\cs -> case cs of
                           []  -> [((),[])]
                           _:_ -> [])

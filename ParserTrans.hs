{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module ParserTrans (GenParser, Parser, 
                   getC,
                   choose,
                   (<|>),
                   satisfy,
                   doParse,  
                   ) where

import Control.Monad.List
import Control.Monad.State

newtype GenParser e a = P ([e] -> [(a, [e])])

type Parser a = GenParser Char a

doParse :: GenParser e a  -> [e] -> [(a,[e])]
doParse p s = undefined

-- | Return the next character
getC :: GenParser e e 
getC = undefined  

-- | Return the next character if it satisfies the given predicate
-- (this was called satP in lecture)
satisfy :: (e -> Bool) -> GenParser e e
satisfy p = undefined 

-- | Combine two parsers together in parallel, producing all 
-- possible results from either parser.                 
choose :: GenParser e a -> GenParser e a -> GenParser e a
choose = undefined

-- | Combine two parsers together in parallel, but only use the 
-- first result. This means that the second parser is used only 
-- if the first parser completely fails. 
(<|>) :: GenParser e a -> GenParser e a -> GenParser e a
p1 <|> p2 = undefined

 


 -- Advanced Programming, HW 5
-- by Hangfei Lin, hangfei
--    Dong Lee（Justin), donghlee

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module Main where

import Control.Monad
import Test.QuickCheck

import Text.PrettyPrint (Doc, (<+>),(<>))
import qualified Text.PrettyPrint as PP

import ParserTrans
import ParserCombinators

import Test.HUnit

type Variable = String

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show, Eq)

data Expression =
    Var Variable
  | Val Value
  | Op  Bop Expression Expression
  deriving (Show, Eq)

data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  deriving (Show, Eq)

data Statement =
    Assign Variable Expression
  | If Expression Statement Statement
  | While Expression Statement
  | Sequence Statement Statement
  | Skip
  deriving (Show, Eq)

-- Problem 0
---------------------------------------------

wFact :: Statement
wFact = Sequence (Assign "N" (Val (IntVal 2))) (Sequence (Assign "F" (Val (IntVal 1))) (While (Op Gt (Var "N") (Val (IntVal 0))) (Sequence (Assign "X" (Var "N")) (Sequence (Assign "Z" (Var "F")) (Sequence (While (Op Gt (Var "X") (Val (IntVal 1))) (Sequence (Assign "F" (Op Plus (Var "Z") (Var "F"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))) (Assign "N" (Op Minus (Var "N") (Val (IntVal 1)))))))))

class PP a where
  pp :: a -> Doc

oneLine :: PP a => a -> IO ()
oneLine = putStrLn . PP.renderStyle (PP.style {PP.mode=PP.OneLineMode}) . pp

indented :: PP a => a -> IO ()
indented = putStrLn . PP.render . pp

instance PP Bop where
  pp Plus   = PP.char '+'
  pp Minus  = PP.char '-'
  pp Times  = PP.char '*'
  pp Divide = PP.char '/'
  pp Gt     = PP.char '>'
  pp Ge     = PP.text ">="
  pp Lt     = PP.char '<'
  pp Le     = PP.text "<="

instance PP Value where
  pp (BoolVal True)   = PP.text "true"
  pp (BoolVal False)  = PP.text "false"
  pp (IntVal v)       = PP.int v

-- | Instance of Expression
instance PP Expression where
  pp (Val v) = pp v
  pp (Var v) = PP.text v
  pp (Op bop exp1 exp2) =
    let dispPP (Op bop' exp1' exp2')  = PP.parens $ dispPP exp1' <+> pp bop' <+> dispPP exp2'
        dispPP expX                   = pp expX
        dispNoP (Op bop' exp1' exp2') = dispPP exp1' <+> pp bop' <+> dispPP exp2'
        dispNoP expX                  = pp expX

        precPrio outerOp innerOp
          | opPrec outerOp > opPrec innerOp  = True
          | otherwise                        = False
      in case (Op bop exp1 exp2) of
        Op bop0 e@(Op bop' _ _) e3 ->
          if precPrio bop0 bop' then
            dispPP e <+> pp bop0 <+> pp e3
          else
            dispNoP e <+> pp bop0 <+> pp e3
        Op bop0 e1 e2              ->
          pp e1 <+> pp bop0 <+> pp e2 
        _                          -> 
          dispPP exp1 <+> pp bop <+> dispPP exp2
            
-- | Operator precedence denoted by interger.
-- Higher int means hihger precedence.
opPrec :: Bop -> Int
opPrec Times  = 3
opPrec Divide = 3
opPrec Plus   = 2
opPrec Minus  = 2
opPrec Gt     = 1
opPrec Ge     = 1
opPrec Lt     = 1
opPrec Le     = 1


-- | Instance of Statement for PP
instance PP Statement where
  pp (Assign var e)     = PP.text var     <+> PP.colon <> PP.equals <+> pp e
  pp (If e stat1 stat2) = PP.text "if"    <+> pp e
                                            <+> PP.text "then"
                                            <+> pp stat1
                                            <+> PP.text "else"
                                            <+> pp stat2
                                            <+> PP.text "endif"
  pp (While e stat)     = PP.text "while" <+> pp e
                                            <+> PP.text "do"
                                            <+> pp stat
                                            <+> PP.text "endwhile"
  pp Skip                 = PP.text "skip"
  pp (Sequence s1 s2)     = pp s1 <+> PP.semi <+> pp s2


display :: PP a => a -> String
display = show . pp

-- Simple tests

oneV, twoV, threeV, fourV :: Expression
oneV   = Val (IntVal 1)
twoV   = Val (IntVal 2)
threeV = Val (IntVal 3)
fourV  = Val (IntVal 4)

xVar,yVar,zVar :: Variable
xVar =  "x"
yVar =  "y"
zVar =  "z"


t0 :: Test
t0 = TestList [display oneV ~?= "1",
      display (BoolVal True) ~?= "true",
      display (Var "X") ~?= "X",
      display (Op Plus oneV twoV) ~?= "1 + 2",
      display (Op Plus oneV (Op Plus twoV threeV)) ~?= "1 + (2 + 3)",
      display (Op Plus (Op Plus oneV twoV) threeV) ~?= "1 + 2 + 3",
      display (Assign "X" threeV) ~?= "X := 3",
      display Skip ~?= "skip"  ]

--- (Your own test cases go here...)

t0b :: Test
t0b  = display (If (Val (BoolVal True)) Skip Skip) ~?=
       "if true then skip else skip endif"

t0b' :: Test
t0b' = display (If (Val (BoolVal True)) Skip Skip) ~?=
      "if true then\n  skip\nelse  skip\nendif"

t01 :: Test
t01 = display (While (Val (BoolVal True)) Skip) ~?=
      "while true do skip endwhile"

t02 :: Test
t02 = display (While (Val (BoolVal True)) Skip) ~?=
      "while true do skip endwhile"

t03 :: Test
t03 = display (While (Val (BoolVal True)) (Assign xVar (Op Plus oneV twoV))) ~?=
      "while true do x := 1 + 2 endwhile"

t04 :: Test
t04 = display (Sequence (Assign xVar (Op Plus oneV twoV))  (Assign xVar (Op Plus oneV twoV))) ~?=
      "x := 1 + 2 ; x := 1 + 2"

t05 :: Test
t05 = display (Sequence (Assign xVar (Op Plus oneV twoV))  (Assign xVar (Op Plus oneV twoV))) ~?=
      "x := 1 + 2 ; x := 1 + 2"

t01_parenthesis :: Test
t01_parenthesis = TestList["s1" ~: display (Op Plus (Op Plus oneV twoV) threeV)                                  ~?= "1 + 2 + 3",
                           "s2" ~: display (Op Plus oneV (Op Plus twoV threeV))                                  ~?= "1 + (2 + 3)",
                           "s4" ~: display (Op Minus (Op Plus oneV twoV) threeV)                                 ~?= "1 + 2 - 3",
                           "s3" ~: display (Op Plus (Op Minus (Op Plus oneV twoV) threeV) (Op Plus oneV threeV)) ~?= "((1 + 2) - 3) + (1 + 3)",
                           "s3" ~: display (Op Plus (Op Times (Op Plus oneV twoV) threeV) (Op Gt oneV threeV))   ~?= "((1 + 2) - 3) + (1 + 3)",
                           "s3" ~: display (Op Plus (Op Gt (Op Plus oneV twoV) threeV) (Op Gt oneV threeV))      ~?= "((1 + 2) - 3) + (1 + 3)"]

t0New :: Test
t0New = TestList [
       display oneV                           ~?= "1",
       display (BoolVal True)                 ~?= "true",
       display (Var "X")                      ~?= "X",
       display (Op Plus oneV twoV)            ~?= "1 + 2",
       display (Op Plus oneV (Op Plus twoV threeV)) `elem` ["1 + (2 + 3)", "1 + 2 + 3"] ~?= True,
       display (Op Plus (Op Plus oneV twoV) threeV) `elem` ["1 + 2 + 3", "(1 + 2) + 3"] ~?= True,
       display (Assign "X" threeV)            ~?= "X := 3",
       display Skip                           ~?= "skip" ]

t01_newPred :: Test
t01_newPred = TestList["s1" ~: display (Op Times (Op Plus oneV twoV) threeV)  ~?= "(1 + 2) * 3",
                       "s1" ~: display (Op Divide (Op Plus oneV twoV) threeV) ~?= "(1 + 2) / 3",
                       "s1" ~: display (Op Times (Op Minus oneV twoV) threeV) ~?= "(1 - 2) * 3",
                       "s1" ~: display (Op Times (Op Gt oneV twoV) threeV)    ~?= "(1 > 2) * 3",
                       "s1" ~: display (Op Times (Op Gt oneV twoV) (Op Gt oneV twoV))       ~?= "(1 > 2) * (1 > 2)",
                       "s1" ~: display (Op Plus (Op Gt oneV twoV) (Op Gt oneV twoV))        ~?= "(1 > 2) + (1 > 2)",
                       "s1" ~: display (Op Plus (Op Gt oneV twoV) (Op Plus oneV twoV))      ~?= "(1 > 2) + 1 + 2",
                       "s1"   ~: display (Op Plus (Op Plus oneV twoV) (Op Gt oneV twoV))    ~?= "1 + 2 + (1 > 2)",
                       "s1"   ~: display (Op Plus (Op Times oneV twoV) (Op Plus oneV twoV)) ~?= "1 * 2 + 1 + 2",
                       "s196" ~: display (Op Times oneV (Op Plus oneV twoV))  ~?= "1 * (1 + 2)",
                       "s197" ~: display (Op Plus oneV (Op Times oneV twoV))  ~?= "1 + 1 * 2",
                       "s198" ~: display (Op Plus (Op Times oneV twoV) oneV ) ~?= "1 * 2 + 1",
                       "s199" ~: display (Op Plus (Op Gt oneV twoV) (Op Times oneV twoV)) ~?= "(1 > 2) + 1 * 2",
                       "s200" ~: display (Op Plus (Op Gt oneV twoV) (Op Times oneV twoV)) ~?= "(1 > 2) + 1 * 2",
                       "s2" ~: display (Op Times (Op Times (Op Plus oneV twoV) threeV) (Op Plus oneV twoV)) ~?= "(1 + 2) * 3 * (1 + 2)",
                       "s2" ~: display (Op Gt (Op Gt oneV oneV) (Op Times (Op Times (Op Plus oneV twoV) threeV) (Op Plus oneV twoV))) ~?= "1 > 1 > ((1 + 2) * 3) * (1 + 2)",
                       "s2" ~: display (Op Plus oneV (Op Gt (Op Times (Op Plus oneV twoV) threeV) (Op Plus oneV twoV)))     ~?= "1 + (((1 + 2) * 3) > (1 + 2))",
                       "s2" ~: display (Op Times oneV (Op Times (Op Times (Op Plus oneV twoV) threeV) (Op Plus oneV twoV))) ~?= "1 * (1 + 2) * 3 * (1 + 2)",
                       "s222" ~: display (Op Times (Op Times (Op Plus oneV twoV) threeV) (Op Plus oneV twoV)) ~?= "(1 + 2) * 3 * (1 + 2)",
                       "s2" ~: display (Op Times (Op Plus oneV twoV) (Op Times (Op Plus oneV twoV) threeV))   ~?= "(1 + 2) * (1 + 2) * 3"]

t01_weird :: Test
t01_weird = TestList["s1" ~: display (Op Plus (Op Plus oneV (Op Times twoV threeV)) fourV) ~?= "1 + 2 * 3 + 4"]

t01_plusparens :: Test
t01_plusparens = TestList["s1" ~: display (Op Plus (Op Plus oneV (Op Plus oneV twoV)) threeV)  ~?= "1 + (1 + 2) + 3",
                          "s2" ~: display (Op Plus (Op Plus oneV twoV) (Op Plus oneV twoV))    ~?= "1 + 2 + (1 + 2)",
                          "s3" ~: display (Op Plus (Op Plus oneV twoV) (Op Plus twoV threeV))  ~?= "1 + 2 + (2 + 3)"]

t01_precedence :: Test
t01_precedence = TestList["s01" ~: display (Op Plus oneV (Op Times twoV threeV))                  ~?= "1 + 2 * 3",
                          "s02" ~: display (Op Times (Op Plus oneV twoV) fourV)                   ~?= "(1 + 2) * 4",
                          "s1" ~: display (Op Times (Op Plus oneV (Op Plus twoV threeV)) fourV)   ~?= "(1 + (2 + 3)) * 4",
                          "s2" ~: display (Op Times (Op Plus oneV twoV) (Op Plus twoV threeV))    ~?= "(1 + 2) * (2 + 3)",
                          "s3" ~: display (Op Plus (Op Times oneV twoV) (Op Plus twoV threeV))    ~?= "1 * 2 + (2 + 3)",
                          "s4" ~: display (Op Plus (Op Plus oneV (Op Times twoV threeV)) fourV)   ~?= "1 + 2 * 3 + 4",
                          "s5" ~: display (Op Times (Op Times oneV (Op Times twoV threeV)) fourV) ~?= "1 * (2 * 3) * 4"]

t01_long_precedence :: Test
t01_long_precedence = TestList["s1" ~: display (Op Divide oneV (Op Times (Op Plus oneV (Op Plus twoV threeV)) fourV)) ~?= "1 / ((1 + (2 + 3)) * 4)",
                          "s2" ~: display (Op Divide oneV (Op Times (Op Plus oneV twoV) (Op Plus twoV threeV)))       ~?= "(1 + 2) * (2 + 3)",
                          "s3" ~: display (Op Divide (Op Times (Op Times oneV twoV) (Op Plus twoV threeV)) oneV)      ~?= "1 * 2 * (2 + 3) / 1",
                          "s4" ~: display (Op Plus (Op Plus oneV (Op Times twoV threeV)) fourV)                       ~?= "1 + 2 * 3 + 4",
                          "s5" ~: display (Op Times (Op Times oneV (Op Times twoV threeV)) fourV)                     ~?= "1 * (2 * 3) * 4"]


------------------------------------------------------------------------
-- Problem 1

valueP :: Parser Value
valueP = intP <|> boolP

intP :: Parser Value
intP = liftM IntVal int

constP :: String -> a -> Parser a
constP s x = do _ <- string s
                _ <- many space
                return x

boolP :: Parser Value
boolP = constP "true" (BoolVal True) <|> constP "false" (BoolVal False)

opP :: Parser Bop 
opP = choice [constP "+"  Plus,
              constP "-"  Minus,
              constP "*"  Times,
              constP "/"  Divide,
              constP ">=" Ge,
              constP ">"  Gt,
              constP "<=" Le,
              constP "<"  Lt
              ]

varP :: Parser Variable
varP = many1 upper

wsP :: Parser a -> Parser a
wsP p = do x <- p
           _ <- many space
           return x

parenP :: Parser a -> Parser a
parenP p = do _ <- string "("
              x <- p
              _ <- string ")"
              return x

addOp :: Parser (Expression -> Expression -> Expression)
addOp = do x <- wsP opP
           case x of 
             Plus  -> return $ Op x
             Minus -> return $ Op x
             _     -> fail "Precedence does not match plus/minus."

mulOp :: Parser (Expression -> Expression -> Expression)
mulOp = do x <- wsP opP
           case x of 
            Times  -> return $ Op x
            Divide -> return $ Op x
            _      -> fail "Precedence does not match times/divide."

boolOp :: Parser (Expression -> Expression -> Expression)
boolOp = do x <- wsP opP
            case x of 
             Gt -> return $ Op x
             Ge -> return $ Op x
             Lt -> return $ Op x
             Le -> return $ Op x
             _  -> fail "Precedence does not match logical operators."

exprValP :: Parser Expression
exprValP = liftM Val (wsP valueP)

exprVarP :: Parser Expression
exprVarP = liftM Var (wsP varP)

exprP :: Parser Expression
exprP = expr where
          expr    = prodP `chainl1` addOp
          prodP   = logicP `chainl1` mulOp
          logicP  = factorP `chainl1` boolOp
          factorP = wsP $ parenP exprP <|> termP
          termP   = exprValP <|> exprVarP

t11 :: Test
t11 = TestList ["s1" ~: succeed (parse exprP "1 "),
                "s2" ~: succeed (parse exprP "1  + 2"),
                "precedence mult" ~: (parse exprP "1 *2 + 3") ~?= Right (Op Plus (Op Times oneV twoV) threeV),
                "With parens" ~: (parse exprP "1 + (2 + 3)") ~?= Right (Op Plus oneV (Op Plus twoV threeV)),
                "With parens and ops" ~: (parse exprP "1 + (2 + 3) + 1") ~?= Right (Op Plus (Op Plus oneV (Op Plus twoV threeV)) oneV)
                    ] where
  succeed (Left _)  = assert False
  succeed (Right _) = assert True

assignP :: Parser Statement
assignP = do v <- wsP varP
             wsP $ constP ":=" ()
             e <- wsP exprP
             return $ Assign v e

ifP :: Parser Statement
ifP = do wsP $ constP "if" ()
         e <- wsP exprP
         wsP $ constP "then" () 
         s1 <- wsP statementP
         wsP $ constP "else" ()
         s2 <- wsP statementP
         wsP $ constP "endif" ()
         return $ If e s1 s2

whileP :: Parser Statement
whileP = do wsP $ constP "while" ()
            e <- wsP exprP
            wsP $ constP "do" ()
            s <- wsP statementP  
            wsP $ constP "endwhile" ()
            return $ While e s 

sequenceP :: Statement -> Parser Statement
sequenceP s = do wsP $ constP ";" ()
                 s' <- wsP statementP
                 return $ Sequence s s'

skipP :: Parser Statement 
skipP = do wsP $ constP "skip" ()
           return Skip

statementP :: Parser Statement
statementP = do s1 <- choice [assignP, ifP, whileP, skipP, parenP statementP]
                sequenceP s1 <|> return s1

t12' :: Test
t12' = TestList []

t12 :: Test
t12 = TestList ["s1" ~: p "fact.imp",
                "s2" ~: p "test.imp", 
                "s3" ~: p "abs.imp" ,
                "s4" ~: p "times.imp" ] where
  p = succeed <=< parseFromFile (liftM2 const statementP eof)                
  --p s = parseFromFile statementP s >>= succeed
    -- Or: p = succeed <=< parseFromFile statementP
  succeed (Left _)  = assert False
  succeed (Right _) = assert True

testRT :: String -> Assertion
testRT filename = do 
   x <- parseFromFile statementP filename 
   case x of 
     Right ast -> case parse statementP (display ast) of
       Right ast' -> assert (ast == ast')
       Left _ -> assert False
     Left _ -> assert False                             

t13 :: Test
t13 = TestList ["s1" ~: testRT "fact.imp",
                "s2" ~: testRT "test.imp", 
                "s3" ~: testRT "abs.imp" ,
                "s4" ~: testRT "times.imp" ]
t14 :: Test
t14 = TestList ["s1" ~: testRT "test.imp" ]

randomVar :: Gen Variable
randomVar = elements $ map (:[]) ['A'..'Z']

instance Arbitrary Value where
  arbitrary = oneof [ liftM IntVal arbitrary, 
                      liftM BoolVal arbitrary ]

instance Arbitrary Bop where
  arbitrary = elements [Plus, Times, Minus, Gt, Ge, Lt, Le]

arbnE :: Int -> Gen Expression
arbnE n = frequency [ (1, liftM Var randomVar), 
                      (1, liftM Val arbitrary),
                      (n, liftM3 Op arbitrary (arbnE (n `div` 2)) (arbnE (n `div` 2))) ]

instance Arbitrary Expression where
  arbitrary = sized arbnE
  shrink (Op op e1 e2) = [e1,e2] 
                      ++ [Op op e1' e2 | e1' <- shrink e1]
                      ++ [Op op e1 e2' | e2' <- shrink e2]     
  shrink (Val i)       = liftM Val $ shrink i
  shrink _             = []

arbnS :: Int -> Gen Statement
arbnS n = frequency [(1, liftM2 Assign randomVar arbitrary),
                     (n, liftM3 If arbitrary (arbnS (n `div` 2)) (arbnS (n `div` 2))),
                     (n, liftM2 While arbitrary (arbnS (n `div` 2))),
                     (n, liftM2 Sequence (arbnS (n `div` 2)) (arbnS (n `div` 2))), 
                     (1, return Skip)]

instance Arbitrary Statement where
  arbitrary = sized arbnS

prop_roundtrip :: Statement -> Bool
prop_roundtrip s = case parse statementP (display s) of 
                        Right s' -> s == s'
                        _        -> False

------------------------------------------------------------------------
-- Problem 2

data Token =
     TokVar String     -- variables
   | TokVal Value      -- primitive values
   | TokBop Bop        -- binary operators
   | Keyword String    -- keywords
      deriving (Eq, Show)

keywords :: [ Parser Token ]
keywords = map (\x -> constP x (Keyword x))
             [ "(", ")", ":=", ";", "if", "then", "else",
             "endif", "while", "do", "endwhile", "skip" ]

type Lexer = Parser [Token]

-- | @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
-- here sep by (many space)
lexer :: Lexer
lexer = sepBy1
        (liftM TokVal valueP <|>
         liftM TokVar varP   <|>
         liftM TokBop opP    <|>
         choice keywords)
        (many space)

-- | Generic parser for int
intGP :: GenParser Token Value
intGP = do element <- getElement
           case element of
              TokVal (IntVal x) -> return (IntVal x)
              _         -> fail ""

-- | Generic parser for boolean variable
boolGP :: GenParser Token Value
boolGP = do element <- getElement
            case element of
              TokVal (BoolVal x)  -> return (BoolVal x)
              _                   -> fail ""

-- | Generic parser for value
valueGP :: GenParser Token Value
valueGP = do element <- getElement
             case element of
                TokVal x  -> return x
                _         -> fail ""

-- | Generic Parser for binary operators
opGP :: GenParser Token Bop
opGP = do element <- getElement
          case element of
                TokBop x  -> return x
                _         -> fail ""

-- | Generic parser
addOpGP :: GenParser Token (Expression -> Expression -> Expression)
addOpGP = do x <- opGP
             case x of
                 Plus  -> return $ Op x
                 Minus -> return $ Op x
                 _     -> fail "Precedence does not match plus/minus."

-- | Generic parser
mulOpGP :: GenParser Token (Expression -> Expression -> Expression)
mulOpGP = do x <- opGP
             case x of
                 Times  -> return $ Op x
                 Divide -> return $ Op x
                 _     -> fail "Precedence does not match plus/minus."

-- | Generic parser
boolOpGP :: GenParser Token (Expression -> Expression -> Expression)
boolOpGP = do x <- opGP
              case x of
                Gt -> return $ Op x
                Ge -> return $ Op x
                Lt -> return $ Op x
                Le -> return $ Op x
                _  -> fail "Precedence does not match logical operators."

-- | Generic parser for expressions
exprGP :: GenParser Token Expression
exprGP = expr where
           expr      =  prodGP   `chainl1` addOpGP
           prodGP    =  logicGP  `chainl1` mulOpGP
           logicGP   =  factorGP `chainl1` boolOpGP
           factorGP  =  parenGP expr <|> termGP
           termGP    =  exprValGP    <|> exprVarGP

-- | Generic parser for parenthesis
parenGP :: GenParser Token a -> GenParser Token a
parenGP p = do elementMatch (Keyword "(")
               x <- p
               elementMatch (Keyword ")")
               return x

e00 :: [Token]
e00 = [
        TokVal (IntVal 2)
      ]

e01 :: [Token]
e01 = [ Keyword "("
      , TokVal (IntVal 2)
      , Keyword ")"
      ]

e02' :: [Token]
e02' =  [ TokVal $ IntVal 2
        , TokBop Plus
        , TokVal $ IntVal 3
        , TokBop Plus
        , TokVal $ IntVal 4
        ]

e02 :: [Token]
e02 =  [
         Keyword "("
       , TokVal $ IntVal 2
       , TokBop Plus
       , TokVal $ IntVal 3
       , Keyword ")"
       ]

exprValGP :: GenParser Token Expression
exprValGP = do x <- getElement
               case x of
                  TokVal c  -> return (Val c)
                  _         -> fail "exprValGP fail"

exprVarGP :: GenParser Token Expression
exprVarGP = do x <- getElement
               case x of
                  TokVar c  -> return (Var c)
                  _         -> fail "exprVarGP fail"

-- | Satisfy
satisfyGP :: (Token -> Bool) -> GenParser Token Token
satisfyGP p = do c <- getElement
                 if (p c) then
                   return c
                 else fail "End of input"

-- | Token element match
elementMatch :: Token ->  GenParser Token ()
elementMatch c = do _ <- satisfyGP (c ==)
                    return ()

varGP :: GenParser Token Variable
varGP = do element <- getElement
           case element of
              TokVar x -> return x
              _         -> error ""

assignGP :: GenParser Token Statement
assignGP = do v <- varGP
              elementMatch (Keyword ":=")
              e <- exprGP
              return $ Assign v e

whileGP :: GenParser Token Statement
whileGP = do elementMatch (Keyword "while")
             e <- exprGP
             elementMatch (Keyword "do")
             s <- statementGP
             elementMatch (Keyword "endwhile")
             return $ While e s


sequenceGP :: Statement -> GenParser Token Statement
sequenceGP s = do elementMatch (Keyword ";")
                  s' <- statementGP
                  return $ Sequence s s'

statementGP :: GenParser Token Statement
statementGP = do s1 <- choice [assignGP, skipGP, ifGP, whileGP, parenGP statementGP]
                 sequenceGP s1 <|> return s1 

skipGP :: GenParser Token Statement
skipGP = do elementMatch (Keyword "skip")
            return Skip

ifGP :: GenParser Token Statement
ifGP = do elementMatch (Keyword "if")
          e  <- exprGP 
          elementMatch (Keyword "then")
          s1 <- statementGP
          elementMatch (Keyword "else")
          s2 <- statementGP
          elementMatch (Keyword "endif")
          return $ If e s1 s2 


prop_groundtrip :: Statement -> Bool
prop_groundtrip s = case parse statementP (display s) of 
                        Right s' -> s == s'
                        _        -> False

-----------------------------------------------------------------
-- A main action to run all the tests...

main :: IO ()
main = do _ <- runTestTT (TestList [ t0])
          return ()

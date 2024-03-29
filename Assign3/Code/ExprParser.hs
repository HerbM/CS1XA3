{-|
Module : ExprParser
Description : Contains parsing information
Copyright : (c) Hariesh Jayanthan @2018 
License : WTFPL
Maintainer : Jayanthh@mcmaster.ca
Stability : experimental
Portability : DOS

-}



module ExprParser where

import           ExprType

import           Text.Parsec
import           Text.Parsec.String



-- * Parsing Integers, Ints, Floats, and Doubles



-- ** Parsing Integers
-- | parseExprInteger uses intgHelper and ConstIntg to parse strings into Expr integers


parseExprInteger:: String -- ^ a string is inputted
            -> Expr Integer -- ^ ends with an Expr type that holds an integer
parseExprInteger arg = case parse addHelperintg "" ("("++arg++")") of 
                    Left err -> error "Invalid Entry"
                    Right addHelper -> addHelper

addHelperintg :: Parser (Expr Integer)
addHelperintg = try (parens $ addHelper addHelperintg) <|> try constIntg  <|> var

constIntg = do {i <- fmap read $ try nDigits <|> many1 digit;
            return (Const i)}


-- ** Parsing Ints
-- | parseExprInt uses intgHelper and ConstInt to parse strings into Expr ints


parseExprInt :: String -- ^ a string is inputted
            -> Expr Int -- ^ ends with an Expr type that holds an int
parseExprInt arg = case parse addHelperInt "" ("("++arg++")") of
                    Left err -> error "Invalid Entry"
                    Right addHelper -> addHelper

addHelperInt :: Parser (Expr Int)
addHelperInt = try (parens $ addHelper addHelperInt) <|> try intConst <|> var

intConst = do {i <- (fmap read $ try nDigits <|> many1 digit);
            return (Const i)}

-- ** Parsing Doubles
-- | parseExprD takes addHelperD and dobConst to parse strings into Expr doubles

parseExprD :: String -- ^ a string is inputted 
			 -> Expr Double -- ^ ends with an Expr type that holds a double value
parseExprD ss = case parse addHelperD "" ("("++ss++")") of
                  Left err   -> error $ show err
                  Right addHelper -> addHelper

addHelperD :: Parser (Expr Double)
addHelperD = try (parens $ addHelper addHelperD) <|> try dobConst <|> var

dobConst = do {d <- fmap read $ try nDigits <|> decimalDigits;
            return (Const d)}


-- ** Parsing Floats
-- | parseExprF uses addHelperF and flConst to parse strings into Expr floats

parseExprF :: String -- ^ a string is inputted
				-> Expr Float -- ^ ends with an Expr type that holds a float

parseExprF ss = case parse addHelperF  "" ("("++ss++")") of
                  Left err   -> error $ show err
                  Right addHelper -> addHelper

addHelperF :: Parser (Expr Float)
addHelperF  = try (parens $ addHelper addHelperF ) <|> try flConst <|> var

flConst = do {f <- fmap read $ try nDigits <|> decimalDigits;
            return (Const f)}


-- * parsing the infix operators of Mult, Add, and Expr, also Trig values

-- | recognizes !* as mult
multIn :: Parser (Expr a -> Expr a -> Expr a)
multIn = do {symbol "!*";
             return (Mult)}

-- | recognizes !+ as Add
addIn :: Parser (Expr a -> Expr a -> Expr a)
addIn = do {symbol "!+";
             return (Add)}

-- | recognizes !^ as Exponent
expIn :: Parser (Expr a -> Expr a -> Expr a)
expIn = do {symbol "!^";
            return (Exponent)}


trigIn :: String ->
            (Expr a -> Expr a) ->
            Parser (Expr a) ->
            Parser (Expr a)
trigIn name val root = do {symbol name;
                            addHelper <- root;
                         return (val addHelper)}

-- * Other Helper Functions: Parens and Symbol

parens :: Parser a -> Parser a
parens p = do { symbol "(";
                cs <- p;
                symbol ")";
                return cs }

-- | allows for the parsing of Strings surrounded by spaces
symbol :: String -> Parser String
symbol ss = let
  symbol' :: Parser String
  symbol' = do { spaces;
                 ss' <- string ss;
                 spaces;
                 return ss' }
  in try symbol'

trig :: Parser (Expr a) -> Parser (Expr a)
trig f = try (trigIn "cos" Cos f) 
            <|> try (trigIn "sin" Sin f) 
            <|> try (trigIn "ln" Ln f) 
            <|> try (trigIn "e^" Exp f)
            <|> f

-- | Helps parse addHelperession seperated by !+ signs
addHelper :: Parser (Expr a) -> Parser (Expr a)
addHelper f = (multHelper f) `chainl1` addIn

multHelper :: Parser (Expr a) -> Parser (Expr a)
multHelper f = (trigH f) `chainl1` multIn

trigH :: Parser (Expr a) -> Parser (Expr a)
trigH f = (trig f) `chainl1` expIn

var :: Parser (Expr a)
var = do {  spaces;
            ss <- many1 alphaNum;
            spaces;
            return (Var ss)}



-- | Allows the parsing of negative digits
nDigits :: Parser String
nDigits = do { neg <- symbol "-" ;
                    dig <- many1 digit ;
                    return (neg ++ dig) }



-- | Allows the parsing of decimal digits
decimalDigits :: Parser String
decimalDigits = do { d <- char '.' ;
                     rm <- many1 digit ;
                     return $ d:rm }

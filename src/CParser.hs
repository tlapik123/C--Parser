module CParser where

import           Data.Char                      ( isAlpha
                                                , isAlphaNum
                                                )
import           Parser

type Name = String

data Type
    = Null
    | Char Char
    | String String
    | Int Int
    | Bool Bool
    | List [Type]
    deriving (Eq, Show)

data ComparisonType
    = Equals -- "=="
    | NotEquals -- "!="
    deriving (Eq, Show)

-- Name ComparisonType Name
data Comparison = Comparison ComparisonType Name Name
    deriving (Eq, Show)

data Op
    = Add -- '-'
    | Sub -- '+'
    | Mul -- '*'
    | Div -- '/'
    deriving (Eq, Show)

data Statement
    = Variable Name Type -- Name = Value;
    | Assgn Name Name Op Name -- Name = Name Op Name;
    | IfElse Comparison [Statement] [Statement] -- if (Comp) {Statements} else {Statements}
    | While Comparison [Statement] -- while (Comp) {Statements}
    | For Statement Comparison Statement [Statement] -- For (Variable; Comp; Assg;) {Statements}
    deriving (Eq, Show)

-- parses quoted char - similar to parseQuotedString
parseQuotedChar :: Parser Char
parseQuotedChar = between (symbol "'") (symbol "'") parseNonquoteChar
    where parseNonquoteChar = satisfy "not a \'" (/= '\'') -- the text of satisfy should never happen

-- parses Name - starts with alpha and continues with alphaNum
parseName :: Parser String
parseName = do
    start <- satisfy "starts with alpha" isAlpha
    rest  <- many $ satisfy "is alphaNum" isAlphaNum
    _     <- spaces
    return (start : rest)

-- parses all of Type data
parseType :: Parser Type
parseType = do
    _ <- spaces
    choice
        "Null or char or string or int or bool or list of types or pointer to type"
        [ Null <$ parseNull
        , Char <$> parseQuotedChar
        , String <$> parseQuotedString
        , Int <$> number
        , Bool <$> parseBool
        , List <$> parseListOf parseType
        ]

-- parses with respect to semicolon at the end
parseWithSemicolon :: Parser a -> Parser a
parseWithSemicolon p = do
    result <- p
    _      <- spaces
    symbol ";"
    return result

-- parses around certain symbol
parseAroundSymbol
    :: Parser a -> Parser b -> String -> (a -> b -> c) -> Parser c
parseAroundSymbol pa pb s fabPc = do
    paRes <- pa
    _     <- spaces
    symbol s
    _     <- spaces
    pbRes <- pb
    _     <- spaces
    return $ fabPc paRes pbRes

-- parses comparisons - So far only Equals or NotEquals
parseComparison :: Parser Comparison
parseComparison = do
    _ <- spaces
    choice
        "equals or not equals"
        [ bindNames "==" (Comparison Equals)
        , bindNames "!=" (Comparison NotEquals)
        ]
    where bindNames = parseAroundSymbol parseName parseName

-- parses something between parenthesis
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- parses something between programmer brackets
programmerBrackets :: Parser a -> Parser a
programmerBrackets = between (symbol "{") (symbol "}")

-- parses Operand
parseOp :: Parser Op
parseOp = do
    _ <- spaces
    choice
        "add, sub, mul or div"
        [Add <$ char '+', Sub <$ char '-', Mul <$ char '*', Div <$ char '/']

-- parses all of the Statements data
parseStatement :: Parser Statement
parseStatement = do
    _ <- spaces
    choice "variable, assignment, ifelse, for or while"
           [parseVariable, parseAssgn, parseIfElse, parseWhile, parseFor]
  where
    statements   = programmerBrackets $ many parseStatement
    compInParens = parens parseComparison

    parseVariable =
        parseWithSemicolon $ parseAroundSymbol parseName parseType "=" Variable

    parseAssgn = parseWithSemicolon $ do
        uncompleteAssgn <- parseAroundSymbol parseName parseName "=" Assgn
        op              <- parseOp
        _               <- spaces
        name2           <- parseName
        _               <- spaces
        return $ uncompleteAssgn op name2

    parseIfElse = do
        symbol "if"
        comp    <- compInParens
        _       <- spaces
        ifStats <- statements
        _       <- spaces
        symbol "else"
        elseStats <- statements
        _         <- spaces
        return $ IfElse comp ifStats elseStats

    parseWhile = do
        symbol "while"
        comp  <- compInParens
        _     <- spaces
        stats <- statements
        _     <- spaces
        return $ While comp stats

    parseFor = do
        symbol "for"
        (assg, comp, assgn) <- parens triplet
        stats               <- statements
        _                   <- spaces
        return $ For assg comp assgn stats
      where
        triplet = do
            assg  <- parseVariable
            _     <- spaces
            comp  <- parseWithSemicolon parseComparison
            _     <- spaces
            assgn <- parseAssgn
            _     <- spaces
            return (assg, comp, assgn)



-- Nakonec jsem to cele nepouzil, ale alespon jsem to doplnil

-- parses or returns Nothing on fail
optional :: Parser a -> Parser (Maybe a)
optional p = (Just <$> p) <|> returnParser Nothing

-- parses left asociative rules
parseLeftAssoc :: Parser a -> Parser (a -> a -> a) -> Parser a
parseLeftAssoc p op = do
    x <- p
    process x
  where
    process x = do
        maybef <- optional op
        case maybef of
            Nothing -> return x
            Just f  -> do
                y <- p
                process (f x y)

-- použití

-- factor = parens expr <|> number
-- term = parseLeftAssoc factor addOp
-- expr = parseLeftAssoc term mulOp

-- parser for addition/subtraction
addOp :: Num a => Parser (a -> a -> a)
addOp = choice "additive / subtractive" [add, sub]
  where
    add = do
        symbol "+"
        return (+)
    sub = do
        symbol "-"
        return (-)

-- it would be wise to split mul and div, because div is contrainted to fractionals
-- parser for multiplication and division
mulOp :: Fractional a => Parser (a -> a -> a)
mulOp = choice "multiplication / division" [mul, div]
  where
    mul = do
        symbol "*"
        return (*)
    div = do
        symbol "/"
        return (/)

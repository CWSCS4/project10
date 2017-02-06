module JackParser where

import Data.Char (isDigit, isAlpha, isSpace)
import Control.Monad (liftM, ap)
import Data.Maybe

data Class
  = Class String [ClassVar] [Subroutine]
  deriving Show
  
data Type 
  = JackInt
  | JackChar
  | JackBool
  | JackClass String
  deriving Show

data ClassVarScope 
  = Static 
  | Field 
  deriving Show

data ClassVar 
  = ClassVar ClassVarScope VarDec
  deriving Show
  
data SubroutineType
  = Method 
  | Constructor 
  | Function 
  deriving Show

data Subroutine
  = Subroutine SubroutineType (Maybe Type) String [Parameter] [VarDec] [Statement]
  deriving Show

data Parameter =
  Parameter Type String
  deriving Show

data VarDec =
  VarDec Type [String]
  deriving Show
  
data Statement
  = Let VarAccess Expression
  | If Expression [Statement] [Statement]
  | While Expression [Statement]
  | Do SubCall
  | Return (Maybe Expression)
  deriving Show

data VarAccess
  = Var String
  | Subscript String Expression
  deriving Show
  
data Expression
  = Expression Term [(Op, Term)]
  deriving Show
  
data Op
  = Plus
  | Minus
  | Times
  | Div
  | And
  | Or
  | LessThan
  | GreaterThan
  | EqualTo
  deriving Show
  
data Term
  = IntConst Int
  | StringConst String
  | Parenthesized Expression
  | BoolConst Bool
  | This
  | Null
  | Access VarAccess
  | SubroutineCall SubCall
  | Unary UnaryOp Term
  deriving Show
  
data SubCall
  = Unqualified String [Expression]
  | Qualified String String [Expression]
  deriving Show

data UnaryOp
  = LogicalNot
  | IntegerNegate
  deriving Show
  
newtype Parser a = Parser (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (Parser f) = f


-- THE FOLLOWING ALLOWS YOU TO USE DO NOTATION.
instance Monad Parser where
  (>>=) = parseAndThen
  return = parseReturn
instance Applicative Parser where
  (<*>) = ap
  pure = return
instance Functor Parser where
  fmap = liftM 

-- PARSING FUNCTIONS WRITTEN FOR LAST HOMEWORK.

parseReturn :: a -> Parser a
parseReturn x =
  Parser $ \s -> Just (x, s)

parseAndThen :: Parser a -> (a -> Parser b) -> Parser b
parseAndThen pa f =
  Parser $ \s ->
    case parse pa s of
      Nothing ->
        Nothing
      Just (a, s') ->
        parse (f a) s' 

satisfies :: (Char -> Bool) -> Parser Char
satisfies c =
  Parser $ \s -> 
    case s of
      [] -> Nothing
      first : rest -> 
        if c first then
          Just (first, rest)
        else
          Nothing
          
keyword :: String -> Parser ()
keyword [] = return ()
keyword (c:cs) = do
  satisfies (c ==)
  keyword cs

parseMap :: (a -> b) -> Parser a -> Parser b
parseMap aToB aParser = do
  a <- aParser
  return (aToB a)

parseMap2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseMap2 f pa pb = do
  a <- pa
  b <- pb
  return (f a b)

-- PARSER FUNCTIONS WRITTEN IN CLASS

classVarScopeParser :: Parser ClassVarScope
classVarScopeParser = 
  choice 
    [ parseMap (\_ -> Static) (keyword "static")
    , parseMap (\_ -> Field) (keyword "field")
    ]
  
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p =
  Parser $ \s ->
    case parse p s of
      Nothing ->
        Just ([], s)
      Just (a, s') ->
        parse (parseMap (a :) (zeroOrMore p)) s'
      

identifier :: Parser String
identifier =
  let
    isValidFirstChar c = 
      isAlpha c || c == '_'
    isValidLaterChar c =
      isValidFirstChar c || isDigit c
  in
    parseMap2 (:) (satisfies isValidFirstChar) (zeroOrMore (satisfies isValidLaterChar))
    
  
jackTypeParser :: Parser Type
jackTypeParser =
  choice
    [ parseMap (\_ -> JackInt)  (keyword "int")
    , parseMap (\_ -> JackChar) (keyword "char")
    , parseMap (\_ -> JackBool) (keyword "boolean")
    , parseMap JackClass identifier
    ]

choice :: [Parser a] -> Parser a
choice [] = Parser $ \s -> Nothing
choice (firstChoice:remainingChoices) =
  Parser $ \s ->
    case parse firstChoice s of
      Nothing ->
        parse (choice remainingChoices) s
      success ->
        success

classVarParser :: Parser ClassVar
classVarParser =
  do
    scope <- classVarScopeParser
    spaceParser
    decs <- varDecParser
    return (ClassVar scope decs)

isNotNL s=
  not (s=='\n')

lineCommentParser =
  do
    keyword "//"
    lineCommentHelper

lineCommentHelper =
  Parser $ \s->
    let afterRemove = dropWhile isNotNL s
    in
      Just ((), afterRemove)

isNotBCEnd s=
  not (s=='*')

blockCommentParser =
  do
    keyword "/*"
    blockCommentHelper

blockCommentHelper =
  Parser $ \s->
    let afterRemove = dropWhile isNotBCEnd s
    in
      if ((afterRemove !! 0 == '*') && (afterRemove !! 0 == '/')) then
        Just ((), drop 2 afterRemove)
      else
        let afterRemove = drop 1 afterRemove
        in Just((), drop 1 afterRemove)

spaceParser :: Parser ()
spaceParser =
  Parser $ \s->
    let afterRemove = dropWhile isSpace s
    in
      if afterRemove == s then Nothing
      else Just ((), afterRemove)

spaceParserOpt :: Parser ()
spaceParserOpt =
  Parser $ \s->
    let afterRemove = dropWhile isSpace s
    in
      Just ((), afterRemove)

varDecParser :: Parser VarDec
varDecParser =
  do
    jType <- jackTypeParser
    spaceParser
    jStrs <- multipleParse identifier
    keyword ";"
    return (VarDec jType jStrs)


multipleParse :: Parser a -> Parser [a]
multipleParse p=
  do
    first <- p
    remaining <- zeroOrMore $ do
        spaceParserOpt
        keyword ","
        spaceParserOpt
        p
    return (first:remaining)

multipleParseOpt :: Parser a -> Parser [a]
multipleParseOpt p=
  choice
    [multipleParse p
    , return []]

variableParserHelper :: Parser VarDec
variableParserHelper =
  do
    spaceParserOpt
    keyword "var"
    spaceParser
    varDecParser

subroutineParser :: Parser Subroutine
subroutineParser =
  do
    sType <- subroutineTypeParser
    spaceParser
    maybeType <- maybeTypeParser
    spaceParser
    str <- identifier
    spaceParserOpt
    keyword "("
    params <- multipleParseOpt parameterParser
    keyword ")"
    spaceParserOpt
    keyword "{"
    vars <- zeroOrMore variableParserHelper
    statement <- multipleStatementsParser
    keyword "}"
    return (Subroutine sType maybeType str params vars statement)

subroutineTypeParser :: Parser SubroutineType
subroutineTypeParser =
    choice 
    [ parseMap (\_ -> Method) (keyword "method")
    , parseMap (\_ -> Constructor) (keyword "constructor")
    , parseMap (\_ -> Function) (keyword "function")
    ]

maybeTypeParser :: Parser (Maybe Type)
maybeTypeParser =
  choice
  [ parseMap (\_ -> Nothing) (keyword "void")
  , parseMap Just jackTypeParser]

multipleStatementsParser :: Parser [Statement]
multipleStatementsParser =
  do
    temp <- zeroOrMore $ do
        spaceParserOpt
        statementParser
    spaceParserOpt
    return temp

statementParser :: Parser Statement
statementParser =
  choice
  [ letParser
  , ifParser
  , whileParser
  , doParser
  , returnParser]

parameterParser :: Parser Parameter
parameterParser =
  do
    jType <- jackTypeParser
    spaceParser
    name <- identifier
    return (Parameter jType name)

returnParser :: Parser Statement
returnParser =
    do
    keyword "return"
    expr <- choice[ do
        spaceParser
        parseMap Just expressionParser, return Nothing ]
    spaceParserOpt
    keyword ";"
    return (Return expr)

letParser :: Parser Statement
letParser =
  do
    keyword "let"
    spaceParser
    vAccess <- varAccessParser
    spaceParserOpt
    keyword "="
    spaceParserOpt
    expr <- expressionParser
    keyword ";"
    return (Let vAccess expr)

ifParser :: Parser Statement
ifParser =
  do
    keyword "if"
    spaceParser
    keyword "("
    spaceParserOpt
    expr <- expressionParser
    spaceParserOpt
    keyword ")"
    spaceParserOpt
    keyword "{"
    spaceParserOpt
    statement <- multipleStatementsParser
    spaceParserOpt
    keyword "}"
    spaceParserOpt
    keyword "{"
    spaceParserOpt
    elseStatement <- multipleStatementsParser
    spaceParserOpt
    keyword "}"
    return (If expr statement elseStatement)

whileParser :: Parser Statement
whileParser =
  do
    keyword "while"
    spaceParser
    keyword "("
    spaceParserOpt
    expr <- expressionParser
    spaceParserOpt
    keyword ")"
    spaceParserOpt
    keyword "{"
    spaceParserOpt
    statement <- multipleStatementsParser
    spaceParserOpt
    keyword "}"
    return (While expr statement)

expressionParser :: Parser Expression
expressionParser =
  do
    term <- termParser
    spaceParserOpt
    rest <- zeroOrMore $ do
      spaceParserOpt
      op <- opParser
      spaceParserOpt
      term <- termParser
      return (op, term)
    return (Expression term rest)

opParser :: Parser Op
opParser =
  choice 
    [ parseMap (\_ -> Plus) (keyword "+")
    , parseMap (\_ -> Minus) (keyword "-")
    , parseMap (\_ -> Times) (keyword "*")
    , parseMap (\_ -> Div) (keyword "/")
    , parseMap (\_ -> And) (keyword "&")
    , parseMap (\_ -> Or) (keyword "|")
    , parseMap (\_ -> LessThan) (keyword "<")
    , parseMap (\_ -> GreaterThan) (keyword ">")
    , parseMap (\_ -> EqualTo) (keyword "=")
    ]

termParser :: Parser Term
termParser =
  choice
    [ parseMap SubroutineCall subCallParser
    , intConstParser
    , stringConstParser
    , parenthesizedParser
    , parseMap Access varAccessParser
    , unaryParser
    , parseMap (\_ -> This) (keyword "this")
    , parseMap (\_ -> Null) (keyword "null")
    , parseMap (\_ -> BoolConst True) (keyword "true")
    , parseMap (\_ -> BoolConst False) (keyword "false")    
    ]

parenthesizedParser :: Parser Term
parenthesizedParser =
  do
    keyword "("
    spaceParserOpt
    expr <- expressionParser
    spaceParserOpt
    keyword ")"
    return (Parenthesized expr)

unaryParser :: Parser Term
unaryParser =
  do
    op <- unaryOpParser
    spaceParserOpt
    term <- termParser
    return (Unary op term)

unaryOpParser :: Parser UnaryOp
unaryOpParser =
  choice
    [ parseMap (\_ -> LogicalNot) (keyword "~")
    , parseMap (\_ -> IntegerNegate) (keyword "-")
    ]

intConstParser :: Parser Term
intConstParser =
  Parser $ \s->
    case s of
      "" -> Nothing
      first:_ ->
        if isDigit first then
          let
            number = read (takeWhile isDigit s)
          in
            Just (IntConst number, dropWhile isDigit s)
        else
          Nothing

stringConstParser :: Parser Term
stringConstParser =
  do
    keyword "\""
    string <- zeroOrMore $
      satisfies $ \f ->
        not (f=='"')
    keyword "\""
    return (StringConst string)

 {-} = IntConst Int
  | StringConst String
  | Parenthesized Expression
  | BoolConst Bool
  | This
  | Null
  | VarAccess
  | SubroutineCall SubCall
  | Unary UnaryOp Term-}

subscriptParser :: Parser VarAccess
subscriptParser =
  do
    name <- identifier
    spaceParserOpt
    keyword "["
    spaceParserOpt
    index<-expressionParser
    spaceParserOpt
    keyword "]"
    return (Subscript name index)

varAccessParser :: Parser VarAccess
varAccessParser =
  choice
    [ subscriptParser
    , parseMap Var identifier]

doParser :: Parser Statement
doParser =
  do
    keyword "do"
    spaceParser
    sub <- subCallParser
    spaceParserOpt
    keyword ";"
    return (Do sub)

subCallParser :: Parser SubCall
subCallParser =
  choice
  [ qualifiedSubCallParser
  , unqualifiedSubCallParser]

unqualifiedSubCallParser :: Parser SubCall
unqualifiedSubCallParser =
  do
    name <- identifier
    spaceParserOpt
    keyword "("
    spaceParserOpt
    expr <- multipleParseOpt expressionParser
    spaceParserOpt
    keyword ")"
    return (Unqualified name expr)

qualifiedSubCallParser :: Parser SubCall
qualifiedSubCallParser =
  do
    superName <- identifier
    spaceParserOpt
    keyword "."
    spaceParserOpt
    name <- identifier
    spaceParserOpt
    keyword "("
    spaceParserOpt
    expr <- multipleParseOpt expressionParser
    spaceParserOpt
    keyword ")"
    return (Qualified superName name expr)

parseClass :: Parser Class
parseClass = do
  keyword "class"
  spaceParser
  name <- identifier
  spaceParserOpt
  keyword "{"
  vars <- zeroOrMore $ do
    spaceParserOpt
    classVarParser
  subroutines <- zeroOrMore $ do
    spaceParserOpt
    subroutineParser
  spaceParserOpt
  keyword "}"
  return (Class name vars subroutines)


getClass mbVal=
  case mbVal of
    Just (x,_) -> x

convertXML classInput =
  let classToParse = getClass classInput
  in
    xmlClass classToParse


xmlClass :: Class -> String
xmlClass (Class name classVars subroutines) =
  let
    toReturn = "<class>\n<keyword>class</keyword>"
  in
    toReturn ++ xmlIdentifier name ++ "\n<symbol>{<symbol>"++ xmlClassVars "\n<classVarDec>" classVars ++ "\n</classVarDec>"++ xmlSubroutines subroutines++"\n<symbol>}</symbol>\n</class>"

xmlIdentifier :: String -> String
xmlIdentifier identifier=
  "\n<identifier>"++identifier++"</identifier>"

xmlClassVars :: String ->[ClassVar] -> String
xmlClassVars previous list=
  if (length list == 0) then take (length previous-length "\n<classVarDec>") previous ++ "\n<symbol>;</symbol>"
  else
    case (list !! 0) of
      ClassVar scope vardecs ->
        xmlClassVars (previous++(xmlScope scope)++(xmlVarDec vardecs)++"\n</classVarDec>\n<classVarDec>") (drop 1 list)

xmlScope :: ClassVarScope -> String
xmlScope (Field) = "\n<keyword>field</keyword>"
xmlScope (Static) = "\n<keyword>static</keyword>"

xmlIdentifierList :: String -> [String]->String
xmlIdentifierList previous input=
  if length input == 0 then take (length previous-length "\n<symbol>,</symbol>") previous
  else
    xmlIdentifierList (previous++"\n<keyword>"++(input !! 0)++ "</keyword>\n<symbol>,</symbol>") (drop 1 input)

xmlVarDec :: VarDec -> String
xmlVarDec input=
  case input of
    VarDec jType names ->
      xmlType jType++xmlIdentifierList "" names

xmlType :: Type -> String
xmlType (JackInt)= "\n<keyword>int</keyword>"
xmlType (JackChar)= "\n<keyword>char</keyword>"
xmlType (JackBool)= "\n<keyword>bool</keyword>"
xmlType (JackClass string)= "\n<keyword>"++string++"</keyword>"

xmlSubroutines :: String -> [Subroutine] -> String
xmlSubroutines previous listSubs =
  if length listSubs == 0 then previous
  else
    xmlSubroutines (previous ++ xmlSubroutinesIndiv (listSubs !! 0)) (drop 1 listSubs)

xmlSubroutinesIndiv :: Subroutine -> String
xmlSubroutinesIndiv (Subroutine sType mType name params vardecs statements)=
  "\n<subroutineDec>" ++ xmlSubroutineType sType ++ xmlMaybeType mType ++ xmlIdentifier name ++ xmlParameter "<symbol>(</symbol><parameterList>" params ++"</parameterList><symbol>)</symbol>"++ xmlVarDec "<varDec>" vardecs ++ "\n<symbol>;</symbol>\n</varDec>"++xmlStatements statements

xmlSubroutineType :: SubroutineType -> String
xmlSubroutineType (Method)="\n<keyword>method</keyword>"
xmlSubroutineType (Constructor)="\n<keyword>constructor</keyword>"
xmlSubroutineType (Function)="\n<keyword>function</keyword>"

xmlMaybeType :: (Maybe Type) -> String
xmlMaybeType input=
  case input of
    Nothing -> "\n<keyword>void<\keyword>"
    Just (value) -> "\n<keyword>"++value++"<\keyword>"

xmlParameter :: String ->[Parameter] -> String
xmlParameter previous params =
  if length params = 0 then take (length previous-length "\n<symbol>,</symbol>") previous
  else
    case params !! 0 of
      Parameter jType name-> xmlParameter (previous++xmlType jType++xmlIdentifier name++"<symbol>,</symbol>") (drop 1 params)

data Subroutine
  = Subroutine SubroutineType (Maybe Type) String [Parameter] [VarDec] [Statement]
  deriving Show
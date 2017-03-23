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
-- Helper function for newlines/line comments
isNotNL s=
  not (s=='\n')
-- Helper function for line comments; calls lineCommentHelper to remove all until newline
lineCommentParser =
  do
    keyword "//"
    lineCommentHelper
-- Uses isNotNL in a loop to drop chars from string
lineCommentHelper =
  Parser $ \s->
    let afterRemove = dropWhile isNotNL s
    in
      Just ((), afterRemove)
-- Helper function for block comments; identifies first char of "*/"
isNotBCEnd s=
  not (s=='*')
-- First stage of block comment parser
blockCommentParser =
  do
    keyword "/*"
    blockCommentHelper
-- Loops until isNotBCEnd is true; if the next char is "/", then end, otherwise continue
blockCommentHelper =
  Parser $ \s->
    let afterRemove = dropWhile isNotBCEnd s
    in
      if (afterRemove !! 1 == '/') then
        Just ((), drop 2 afterRemove)
      else
        let afterRemove' = drop 1 afterRemove
        in parse blockCommentHelper afterRemove'
-- Helper function for spaceParser
spaceParserHelper :: Parser ()
spaceParserHelper =
  Parser $ \s->
    let afterRemove = dropWhile isSpace s
    in
      if afterRemove == s then Nothing
      else Just ((), afterRemove)
-- Parses spaces, line comments and block comments. At least one required.
spaceParser :: Parser ()
spaceParser = do
  (choice [lineCommentParser,
   blockCommentParser,
    spaceParserHelper])
  zeroOrMore (choice [lineCommentParser,
    blockCommentParser,
    spaceParserHelper])
  return ()

-- Optional version of spaceParser
spaceParserOpt :: Parser ()
spaceParserOpt = do
  zeroOrMore (choice [lineCommentParser,
    blockCommentParser,
    spaceParserHelper])
  return ()
-- Parses var decs
varDecParser :: Parser VarDec
varDecParser =
  do
    jType <- jackTypeParser
    spaceParser
    jStrs <- multipleParse identifier
    keyword ";"
    return (VarDec jType jStrs)

-- Looks for comma seperated items and parses them
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
-- Optional version of multipleParse; returns empty array if nothing is found.
multipleParseOpt :: Parser a -> Parser [a]
multipleParseOpt p=
  choice
    [multipleParse p
    , return []]
-- Helper function ofr varDecParser
variableParserHelper :: Parser VarDec
variableParserHelper =
  do
    spaceParserOpt
    keyword "var"
    spaceParser
    varDecParser
--Parses subroutines and corresponding syntax
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
-- Parses type of subroutine based on keywords in the below choice statement.
subroutineTypeParser :: Parser SubroutineType
subroutineTypeParser =
    choice 
    [ parseMap (\_ -> Method) (keyword "method")
    , parseMap (\_ -> Constructor) (keyword "constructor")
    , parseMap (\_ -> Function) (keyword "function")
    ]
-- Handles voids/types
maybeTypeParser :: Parser (Maybe Type)
maybeTypeParser =
  choice
  [ parseMap (\_ -> Nothing) (keyword "void")
  , parseMap Just jackTypeParser]
-- Handles multiple statements by calling statementParser mutliple times
multipleStatementsParser :: Parser [Statement]
multipleStatementsParser =
  do
    temp <- zeroOrMore $ do
        spaceParserOpt
        statementParser
    spaceParserOpt
    return temp
-- Goes to one of 5 statements
statementParser :: Parser Statement
statementParser =
  choice
  [ letParser
  , ifParser
  , whileParser
  , doParser
  , returnParser]
-- Parses a type and name
parameterParser :: Parser Parameter
parameterParser =
  do
    jType <- jackTypeParser
    spaceParser
    name <- identifier
    return (Parameter jType name)
-- Can return either a variable or nothing (void return)
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
-- Handles let statements
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
-- Handles if statements
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
    elseStatement <- multipleStatementsParser
    spaceParserOpt
    return (If expr statement elseStatement)
-- Handles while loops
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
-- Handles expressions (single)
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
-- Simple op handling by looking up the listed keywords
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
-- Looks through subcalls, int consts, string consts, etc. to determine parsed term.
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
-- Looks thorugh a parenthesized element
parenthesizedParser :: Parser Term
parenthesizedParser =
  do
    keyword "("
    spaceParserOpt
    expr <- expressionParser
    spaceParserOpt
    keyword ")"
    return (Parenthesized expr)
-- Handles an op and a term to be applied to that op
unaryParser :: Parser Term
unaryParser =
  do
    op <- unaryOpParser
    spaceParserOpt
    term <- termParser
    return (Unary op term)
-- Not/Negate
unaryOpParser :: Parser UnaryOp
unaryOpParser =
  choice
    [ parseMap (\_ -> LogicalNot) (keyword "~")
    , parseMap (\_ -> IntegerNegate) (keyword "-")
    ]
-- Int constants; takes all chars until it finds a nondigit char
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
-- Looks until a '"' is found
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
-- Parses subscripts (arrays)
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
-- Can either be an array (subscriptParser), or its own element (parsemap Var identifier)
varAccessParser :: Parser VarAccess
varAccessParser =
  choice
    [ subscriptParser
    , parseMap Var identifier]
-- Do handling
doParser :: Parser Statement
doParser =
  do
    keyword "do"
    spaceParser
    sub <- subCallParser
    spaceParserOpt
    keyword ";"
    return (Do sub)
-- Looks for either a qualified or unqualified SubCall
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
  spaceParserOpt
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


-- haskell objects->xml parsing is usually handled by "passing back" strings from several nested calls.


-- Handles conversion from Maybe type
getClass mbVal=
  case mbVal of
    Just (x,_) -> x
-- Runs xmlClass on a given classToParse
convertXML classInput =
  let classToParse = getClass classInput
  in
    xmlClass classToParse

-- Outer class tags
xmlClass :: Class -> String
xmlClass (Class name classVars subroutines) =
  let
    toReturn = "<class>\n<keyword> class </keyword>"
  in
    toReturn ++ xmlIdentifier name ++ "\n<symbol> { </symbol>"++ xmlClassVars "\n<classVarDec>" classVars++ xmlSubroutines "" subroutines++"\n<symbol> } </symbol>\n</class>"
-- bools, this and null are handled differently than regular identifiers.
xmlIdentifier :: String -> String
xmlIdentifier identifier=
  if ((identifier == "false") || (identifier == "true") || (identifier == "this") || (identifier == "null")) then
    "\n<keyword> "++identifier++" </keyword>"
  else "\n<identifier> "++identifier++" </identifier>"
-- Takes any class vars (if there are any), and sends them to xmlVarDec recursively. If there are none left, then the preceding class var dec tag is removed.
xmlClassVars :: String ->[ClassVar] -> String
xmlClassVars previous list=
  if (length list == 0) then take (length previous-length "\n<classVarDec>") previous
  else
    case (list !! 0) of
      ClassVar scope vardecs ->
        xmlClassVars (previous++(xmlScope scope)++(xmlVarDec vardecs)++"\n<symbol> ; </symbol>\n</classVarDec>\n<classVarDec>") (drop 1 list)

-- Recursively looks thorugh a list of VarDec s.
xmlVarDecs :: String ->[VarDec] -> String
xmlVarDecs previous list=
  if (length list == 0) then previous
  else
    case (list !! 0) of
      varDecObj ->
        xmlVarDecs (previous++"\n<varDec>\n<keyword> var </keyword>"++(xmlVarDec varDecObj)++"\n<symbol> ; </symbol>\n</varDec>") (drop 1 list)

-- Simple scope pattern matching
xmlScope :: ClassVarScope -> String
xmlScope (Field) = "\n<keyword> field </keyword>"
xmlScope (Static) = "\n<keyword> static </keyword>"
-- Looks through a list of identifiers recursively. Preceding symbol-comma tags are removed if last element in list.
xmlIdentifierList :: String -> [String]->String
xmlIdentifierList previous input=
  if length input == 0 then take (length previous-length "\n<symbol> , </symbol>") previous
  else
    xmlIdentifierList (previous++"\n<identifier> "++(input !! 0)++ " </identifier>\n<symbol> , </symbol>") (drop 1 input)
-- Single var dec handling-> gets sent to xmlIdentifierList to handle multiple declarations
xmlVarDec :: VarDec -> String
xmlVarDec input=
  case input of
    VarDec jType names ->
      xmlType jType++xmlIdentifierList "" names
-- Can either be a primitive (hardcoded below), or a new class (which gets an <identifier>class</identifier> tag)
xmlType :: Type -> String
xmlType (JackInt)= "\n<keyword> int </keyword>"
xmlType (JackChar)= "\n<keyword> char </keyword>"
xmlType (JackBool)= "\n<keyword> boolean </keyword>"
xmlType (JackClass string)= "\n<identifier> "++string++" </identifier>"
-- Recursively looks through a list of subroutines
xmlSubroutines :: String -> [Subroutine] -> String
xmlSubroutines previous listSubs =
  if length listSubs == 0 then previous
  else
    xmlSubroutines (previous ++ xmlSubroutinesIndiv (listSubs !! 0)) (drop 1 listSubs)
-- Individual subroutine handling.
xmlSubroutinesIndiv :: Subroutine -> String
xmlSubroutinesIndiv (Subroutine sType mType name params vardecs statements)=
  "\n<subroutineDec>" ++ xmlSubroutineType sType ++ xmlMaybeType mType ++ xmlIdentifier name ++ "\n<symbol> ( </symbol>"++xmlParameter "\n<parameterList>" params ++"\n<symbol> ) </symbol>\n<subroutineBody>\n<symbol> { </symbol>"++  xmlVarDecs "" vardecs ++xmlStatements "\n<statements>" statements++"\n</statements>\n<symbol> } </symbol>\n</subroutineBody>\n</subroutineDec>"
-- Pattern matching for subroutine type.
xmlSubroutineType :: SubroutineType -> String
xmlSubroutineType (Method)="\n<keyword> method </keyword>"
xmlSubroutineType (Constructor)="\n<keyword> constructor </keyword>"
xmlSubroutineType (Function)="\n<keyword> function </keyword>"
-- Handles void vs. regular type.
xmlMaybeType :: (Maybe Type) -> String
xmlMaybeType input=
  case input of
    Nothing -> "\n<keyword> void </keyword>"
    Just (value) -> xmlType value
-- Handles a list of parameters. Like before, preceding symbol-comma tags are taken out of the return string if on last element
xmlParameter :: String ->[Parameter] -> String
xmlParameter previous params =
  if ((length params == 0)) then
    (if not (previous == "\n<parameterList>") then take (length previous-length "\n<symbol> , </symbol>") previous
    else previous)++ "\n</parameterList>"
  else
    case params !! 0 of
      Parameter jType name-> xmlParameter (previous++xmlType jType++xmlIdentifier name++"\n<symbol> , </symbol>") (drop 1 params)
-- Muliple xml statements, recursively run on xmlStatementsIndiv
xmlStatements :: String -> [Statement] -> String
xmlStatements previous statements =
  if length statements == 0 then previous
  else
    xmlStatements (previous++(xmlStatementsIndiv (statements !! 0))) (drop 1 statements)
-- Var acces with both regular variables and arrays.
xmlVarAccess :: VarAccess -> String
xmlVarAccess (Var string)= xmlIdentifier string
xmlVarAccess (Subscript string expr)=
  xmlIdentifier string ++ "\n<symbol> [ </symbol>" ++ xmlExpression expr ++ "\n<symbol> ] </symbol>"
-- Expression handling
xmlExpression :: Expression -> String
xmlExpression expr =
  case expr of
    Expression term listOpTerms -> "\n<expression>\n<term>"++xmlTerm term++"\n</term>"++xmlOpTerms listOpTerms++"\n</expression>"
-- Handles op-term combinations, recuesively calls itself if there are more op-terms enclosed within the current op-term.
xmlOpTerms :: [(Op, Term)] -> String
xmlOpTerms [] = ""
xmlOpTerms ((op, term):opTermsRem) =
  if length opTermsRem == 0
    then xmlOp op ++ "\n<term>"++xmlTerm term++"\n</term>"
  else
    xmlOp op ++ "\n<term>"++xmlTerm term++"\n</term>" ++ (if (not (length opTermsRem==0))
      then "\n<symbol> , <symbol>"
      else "") ++ xmlOpTerms opTermsRem

-- Terms (mostly through simple pattern matching)
xmlTerm :: Term -> String
xmlTerm (IntConst int)= "\n<integerConstant> "++ show int ++ " </integerConstant>"
xmlTerm (StringConst string)= "\n<stringConstant> " ++ string ++ " </stringConstant>"
xmlTerm (Parenthesized expr)= "\n<symbol> ( </symbol>"++xmlExpression expr++ "\n<symbol> ) </symbol>"
xmlTerm (This)= "\n<keyword> this </keyword>"
xmlTerm (Null)= "\n<keyword> null </keyword>"
xmlTerm (Access vAccess)= xmlVarAccess vAccess
xmlTerm (SubroutineCall subCall)= xmlSubcall subCall
xmlTerm (Unary unaryOp term)= xmlUnaryOp unaryOp ++ "\n<term>"++xmlTerm term++"\n</term>"
-- Not/Negate
xmlUnaryOp :: UnaryOp -> String
xmlUnaryOp (LogicalNot)="\n<symbol> ~ </symbol>"
xmlUnaryOp (IntegerNegate)="\n<symbol> - </symbol>"
-- Individual statement handling, separate calls for Let, If, While, Do and Return statements.
xmlStatementsIndiv :: Statement -> String
xmlStatementsIndiv (Let vAccess expr) =
  "\n<letStatement>\n<keyword> let </keyword>" ++ xmlVarAccess vAccess ++ "\n<symbol> = </symbol>" ++xmlExpression expr ++ "\n<symbol> ; </symbol>\n</letStatement>"

xmlStatementsIndiv (If expr thenList elseList) =
  "\n<ifStatement>\n<keyword> if </keyword>\n<symbol> ( </symbol>"++xmlExpression expr++"\n<symbol> ) </symbol>\n<symbol> { </symbol>\n<statements>"++xmlStatements "" thenList++"\n</statements>\n<symbol> } </symbol>\n</ifStatement>"++xmlStatements "" elseList

xmlStatementsIndiv (While expr whileList) =
  "\n<whileStatement>\n<keyword> while </keyword>\n<symbol> ( </symbol>"++xmlExpression expr++"\n<symbol> ) </symbol>\n<symbol> { </symbol>\n<statements>"++xmlStatements "" whileList++"\n</statements>\n<symbol> } </symbol>\n</whileStatement>"

xmlStatementsIndiv (Do subCall) =
  "\n<doStatement>\n<keyword> do </keyword>"++xmlSubcall subCall++"\n<symbol> ; </symbol>\n</doStatement>"

xmlStatementsIndiv (Return maybeExpr) =
  "\n<returnStatement>"++xmlReturn maybeExpr++"</returnStatement>"
-- Simple op terms
xmlOp :: Op -> String
xmlOp Plus = "\n<symbol> + </symbol>"
xmlOp Minus = "\n<symbol> - </symbol>"
xmlOp Times = "\n<symbol> *< /symbol>"
xmlOp Div = "\n<symbol> / </symbol>"
xmlOp And = "\n<symbol> &amp; </symbol>"
xmlOp Or = "\n<symbol> | </symbol>"
xmlOp LessThan = "\n<symbol> &lt; </symbol>"
xmlOp GreaterThan = "\n<symbol> &gt; </symbol>"
xmlOp EqualTo = "\n<symbol> = </symbol>"
-- Subcalls, for both unqualified and qualified calls
xmlSubcall :: SubCall -> String
xmlSubcall (Unqualified string exprList) =
  xmlIdentifier string ++ "\n<symbol> ( </symbol>\n<expressionList>"++xmlExpressionList exprList++"\n</expressionList>\n<symbol> ) </symbol>"
xmlSubcall (Qualified string string2 exprList) =
  xmlIdentifier string ++ "\n<symbol> . </symbol>"++xmlIdentifier string2++"\n<symbol> ( </symbol>\n<expressionList>"++xmlExpressionList exprList++"\n</expressionList>\n<symbol> ) </symbol>"
-- Recursively goes through a list of expressions
xmlExpressionList :: [Expression]->String
xmlExpressionList [] = ""
xmlExpressionList (first:remaining) = xmlExpression first ++ (if length remaining > 0 then "\n<symbol> , </symbol>" else "")++xmlExpressionList remaining
--Return handling, based on whether input (return type) is Nothing or not. (void vs. other return type.)
xmlReturn :: (Maybe Expression)->String
xmlReturn input=
  case input of
    Nothing -> "\n<keyword> return </keyword>\n<symbol> ; </symbol>"
    Just (value) -> "\n<keyword> return </keyword>"++(if isNothing input then
      ""
      else
        xmlExpression (fromJust input))++"\n<symbol> ; </symbol>"
module JackParser where

import Data.Char (isDigit, isAlpha, isSpace)
import Data.List (intercalate, intersperse)
import Control.Monad (liftM, ap)
import Xml (Xml (TextNode, XmlNode), Xmlable, XmlArrayable, toXml, toXmlArray, flatten)

keywordNode :: String -> Xml
keywordNode = TextNode "keyword"

identifierNode :: String -> Xml
identifierNode = TextNode "identifier"

symbolNode :: String -> Xml
symbolNode = TextNode "symbol"

data Class
  = Class String [ClassVar] [Subroutine]
instance Xmlable Class where
  toXml (Class name classVars subroutines) =
    XmlNode "class"
      (
        [ keywordNode "class"
        , identifierNode name
        , symbolNode "{"
        ] ++
        map toXml classVars ++
        map toXml subroutines ++
        [symbolNode "}"]
      )

data Type
  = JackInt
  | JackChar
  | JackBool
  | JackClass String
instance Xmlable Type where
  toXml JackInt = keywordNode "int"
  toXml JackChar = keywordNode "char"
  toXml JackBool = keywordNode "boolean"
  toXml (JackClass className) = identifierNode className

data ClassVarScope
  = Static
  | Field
instance Xmlable ClassVarScope where
  toXml Static = keywordNode "static"
  toXml Field = keywordNode "field"

data ClassVar
  = ClassVar ClassVarScope VarDec
instance Xmlable ClassVar where
  toXml (ClassVar scope dec) =
    XmlNode "classVarDec"
      (toXml scope : toXmlArray dec)

data SubroutineType
  = Method
  | Constructor
  | Function
instance Xmlable SubroutineType where
  toXml subroutineType =
    let
      typeString = case subroutineType of
        Method -> "method"
        Constructor -> "constructor"
        Function -> "function"
    in
      keywordNode typeString

data Subroutine
  = Subroutine SubroutineType (Maybe Type) String [Parameter] [VarDec] [Statement]
instance Xmlable Subroutine where
  toXml (Subroutine subroutineType returnType name parameters localVars body) =
    let
      returnTypeXml = case returnType of
        Nothing -> keywordNode "void"
        Just jackType -> toXml jackType
      varDecToXml dec =
        XmlNode "varDec"
          (keywordNode "var" : toXmlArray dec)
    in
      XmlNode
        "subroutineDec"
        (
          [ toXml subroutineType
          , returnTypeXml
          , identifierNode name
          , symbolNode "("
          , XmlNode "parameterList"
            (intercalate [symbolNode ","] (map toXmlArray parameters))
          , symbolNode ")"
          , XmlNode "subroutineBody"
            (
              [symbolNode "{"] ++
              map varDecToXml localVars ++
              [ toXml body
              , symbolNode "}"
              ]
            )
          ]
        )

data Parameter =
  Parameter Type String
instance XmlArrayable Parameter where
  toXmlArray (Parameter paramType name) =
    [ toXml paramType
    , identifierNode name
    ]

data VarDec =
  VarDec Type [String]
instance XmlArrayable VarDec where
  toXmlArray (VarDec varType names) =
    [toXml varType] ++
    intersperse (symbolNode ",") (map identifierNode names) ++
    [symbolNode ";"]

data Statement
  = Let VarAccess Expression
  | If Expression [Statement] [Statement]
  | While Expression [Statement]
  | Do SubCall
  | Return (Maybe Expression)
instance Xmlable Statement where
  toXml (Let access expression) =
    XmlNode "letStatement"
      (
        [keywordNode "let"] ++
        toXmlArray access ++
        [ symbolNode "="
        , toXml expression
        , symbolNode ";"
        ]
      )
  toXml (If expression ifBlock elseBlock) =
    let
      elseBlockXml = case elseBlock of
        [] -> []
        _ ->
          [ keywordNode "else"
          , symbolNode "{"
          , toXml elseBlock
          , symbolNode "}"
          ]
    in
      XmlNode "ifStatement"
        (
          [ keywordNode "if"
          , symbolNode "("
          , toXml expression
          , symbolNode ")"
          , symbolNode "{"
          , toXml ifBlock
          , symbolNode "}"
          ] ++
          elseBlockXml
        )
  toXml (While expression block) =
    XmlNode "whileStatement"
      [ keywordNode "while"
      , symbolNode "("
      , toXml expression
      , symbolNode ")"
      , symbolNode "{"
      , toXml block
      , symbolNode "}"
      ]
  toXml (Do subCall) =
    XmlNode "doStatement"
      (
        [keywordNode "do"] ++
        toXmlArray subCall ++
        [symbolNode ";"]
      )
  toXml (Return maybeExpression) =
    let
      expressionXml =
        case maybeExpression of
          Nothing ->
            []
          Just expression ->
            [toXml expression]
    in
      XmlNode "returnStatement"
        (
          [keywordNode "return"] ++
          expressionXml ++ 
          [symbolNode ";"]
        )
instance Xmlable [Statement] where
  toXml statements =
    XmlNode "statements" (map toXml statements)

data VarAccess
  = Var String
  | Subscript String Expression
instance XmlArrayable VarAccess where
  toXmlArray (Var name) = [identifierNode name]
  toXmlArray (Subscript name expression) =
    [ identifierNode name
    , symbolNode "["
    , toXml expression
    , symbolNode "]"
    ]

data Expression
  = Expression Term [(Op, Term)]
instance Xmlable Expression where
  toXml (Expression firstTerm opTerms) =
    let
      opTermToXml (op, term) = [toXml op, toXml term]
      remainingTermsXml = map opTermToXml opTerms
    in
      XmlNode "expression"
        (toXml firstTerm : flatten remainingTermsXml)

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
instance Xmlable Op where
  toXml op = 
    let
      symbol = case op of
        Plus -> "+"
        Minus -> "-"
        Times -> "*"
        Div -> "/"
        And -> "&"
        Or -> "|"
        LessThan -> "<"
        GreaterThan -> ">"
        EqualTo -> "="
    in
      symbolNode symbol

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
instance Xmlable Term where
  toXml (IntConst int) =
    XmlNode "term"
      [TextNode "integerConstant" (show int)]
  toXml (StringConst string) =
    XmlNode "term"
      [TextNode "stringConstant" string]
  toXml (Parenthesized expression) =
    XmlNode "term"
      [ symbolNode "("
      , toXml expression
      , symbolNode ")"
      ]
  toXml (BoolConst bool) =
    let
      boolKeyword =
        if bool then "true"
        else "false"
    in
      XmlNode "term"
        [keywordNode boolKeyword]
  toXml This =
    XmlNode "term"
      [keywordNode "this"]
  toXml Null =
    XmlNode "term"
      [keywordNode "null"]
  toXml (Access access) =
    XmlNode "term"
      (toXmlArray access)
  toXml (SubroutineCall subCall) =
    XmlNode "term"
      (toXmlArray subCall)
  toXml (Unary op term) =
    XmlNode "term"
      [ toXml op
      , toXml term
      ]

data SubCall
  = Unqualified String [Expression]
  | Qualified String String [Expression]
instance XmlArrayable SubCall where
  toXmlArray (Unqualified name expressions) =
    [ identifierNode name
    , symbolNode "("
    , XmlNode "expressionList"
      (intersperse (symbolNode ",") (map toXml expressions))
    , symbolNode ")"
    ]
  toXmlArray (Qualified namespace name expressions) =
    [ identifierNode namespace
    , symbolNode "."
    ] ++
    toXmlArray (Unqualified name expressions)

data UnaryOp
  = LogicalNot
  | IntegerNegate
instance Xmlable UnaryOp where
  toXml LogicalNot = symbolNode "~"
  toXml IntegerNegate = symbolNode "-"

newtype Parser a = Parser (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (Parser f) = f

instance Monad Parser where
  (>>=) = parseAndThen
  return = parseReturn
instance Applicative Parser where
  (<*>) = ap
  pure = return
instance Functor Parser where
  fmap = liftM

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
  _ <- satisfies (c ==)
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

parseKeywordValue :: [(String, a)] -> Parser a
parseKeywordValue nameValues =
  let
    getParser (name, value) =
      parseMap (const value) (keyword name)
  in
    choice (map getParser nameValues)

parseClassVarScope :: Parser ClassVarScope
parseClassVarScope =
  parseKeywordValue
    [ ("static", Static)
    , ("field", Field)
    ]

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore parser =
  Parser $ \string ->
    case parse parser string of
      Nothing ->
        Just ([], string)
      Just (aValue, remaining) ->
        parse (parseMap (aValue :) (zeroOrMore parser)) remaining

oneOrMore :: Parser a -> Parser [a]
oneOrMore parser = parseMap2 (:) parser (zeroOrMore parser)

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
    [ parseKeywordValue
      [ ("int", JackInt)
      , ("char", JackChar)
      , ("boolean", JackBool)
      ]
    , parseMap JackClass identifier
    ]

choice :: [Parser a] -> Parser a
choice [] = parseFail
choice (firstChoice : remainingChoices) =
  Parser $ \string ->
    case parse firstChoice string of
      Nothing ->
        parse (choice remainingChoices) string
      success ->
        success

parseUntil :: Parser a -> Parser a
parseUntil parser =
  Parser $ \string ->
    let parsed = parse parser string
    in
      case parsed of
        Nothing ->
          case string of
            "" ->
              Nothing
            _ : remaining ->
              parse (parseUntil parser) remaining
        _ ->
          parsed

parseEnd :: Parser ()
parseEnd =
  Parser $ \string ->
    case string of
      "" -> Just ((), "")
      _ -> Nothing

isNewLine :: Char -> Bool
isNewLine c = c == '\r' || c == '\n'

parseLineComment :: Parser ()
parseLineComment = do
  keyword "//"
  parseUntil $ choice
    [ parseMap (const ()) (satisfies isNewLine)
    , parseEnd
    ]

parseBlockComment :: Parser ()
parseBlockComment = do
  keyword "/*"
  parseUntil (keyword "*/")

whiteSpaceParser :: Parser ()
whiteSpaceParser =
  Parser $ \string ->
    let dropped = dropWhile isSpace string
    in
      if string == dropped then Nothing -- some whitespace required
      else Just ((), dropped)

requiredSpaceParser :: Parser ()
requiredSpaceParser = do
  _ <- oneOrMore $ choice
    [ whiteSpaceParser
    , parseLineComment
    , parseBlockComment
    ]
  return ()

optionalSpaceParser :: Parser ()
optionalSpaceParser =
  parseMap (const ()) (parseOptional requiredSpaceParser)

parseCommaSeparated :: Parser a -> Parser [a]
parseCommaSeparated parser = do
  first <- parser
  remaining <- zeroOrMore $ do
    optionalSpaceParser
    keyword ","
    optionalSpaceParser
    parser
  return (first : remaining)

optionalParseCommaSeparated :: Parser a -> Parser [a]
optionalParseCommaSeparated parser =
  parseMap resolveMaybeList $
    parseOptional $
      parseCommaSeparated parser

parseFail :: Parser a
parseFail = Parser (\_ -> Nothing)

parseVarDec :: Parser VarDec
parseVarDec = do
  jackType <- jackTypeParser
  requiredSpaceParser
  vars <- parseCommaSeparated identifier
  optionalSpaceParser
  keyword ";"
  return (VarDec jackType vars)

parseClassVar :: Parser ClassVar
parseClassVar = do
  scope <- parseClassVarScope
  requiredSpaceParser
  dec <- parseVarDec
  return (ClassVar scope dec)

parseOptional :: Parser a -> Parser (Maybe a)
parseOptional parser =
  choice
    [ parseMap Just parser
    , return Nothing
    ]

parseIntConstant :: Parser Term
parseIntConstant =
  Parser $ \string ->
    case string of
      "" ->
        Nothing
      firstChar : _ ->
        if isDigit firstChar then
          let readValue = read (takeWhile isDigit string)
          in
            if readValue <= 32767 then
              Just (IntConst readValue, dropWhile isDigit string)
            else Nothing
        else Nothing

parseStringConstant :: Parser Term
parseStringConstant = do
  keyword "\""
  string <- zeroOrMore $
    satisfies $ \c ->
      not ((isNewLine c) || c == '"')
  keyword "\""
  return (StringConst string)

parseSubscript :: Parser VarAccess
parseSubscript = do
  varName <- identifier
  optionalSpaceParser
  keyword "["
  optionalSpaceParser
  index <- parseExpression
  optionalSpaceParser
  keyword "]"
  return (Subscript varName index)

parseAccess :: Parser VarAccess
parseAccess =
  choice
    [ parseSubscript
    , parseMap Var identifier -- this must come after array access because it can start that expression
    ]

parseParenthesized :: Parser Term
parseParenthesized = do
  keyword "("
  optionalSpaceParser
  expression <- parseExpression
  keyword ")"
  return (Parenthesized expression)

parseUnqualifiedSubCall :: Parser SubCall
parseUnqualifiedSubCall = do
  name <- identifier
  optionalSpaceParser
  keyword "("
  optionalSpaceParser
  arguments <- optionalParseCommaSeparated parseExpression
  optionalSpaceParser
  keyword ")"
  return (Unqualified name arguments)

parseQualifiedSubCall :: Parser SubCall
parseQualifiedSubCall = do
  classOrVarName <- identifier
  optionalSpaceParser
  keyword "."
  optionalSpaceParser
  Unqualified callName arguments <- parseUnqualifiedSubCall
  return (Qualified classOrVarName callName arguments)

parseSubCall :: Parser SubCall
parseSubCall =
  choice
    [ parseUnqualifiedSubCall
    , parseQualifiedSubCall
    ]

parseUnaryOp :: Parser UnaryOp
parseUnaryOp =
  parseKeywordValue
    [ ("~", LogicalNot)
    , ("-", IntegerNegate)
    ]

parseUnaryOperation :: Parser Term
parseUnaryOperation = do
  op <- parseUnaryOp
  optionalSpaceParser
  term <- parseTerm
  return (Unary op term)

parseTerm :: Parser Term
parseTerm =
  choice
    [ parseIntConstant
    , parseStringConstant
    , parseKeywordValue
      [ ("true", BoolConst True)
      , ("false", BoolConst False)
      , ("null", Null)
      , ("this", This)
      ]
    , parseMap SubroutineCall parseSubCall
    , parseMap Access parseAccess -- this must come after array subroutine call because a variable access can start that expression
    , parseParenthesized
    , parseUnaryOperation
    ]

parseOp :: Parser Op
parseOp =
  parseKeywordValue
    [ ("+", Plus)
    , ("-", Minus)
    , ("*", Times)
    , ("/", Div)
    , ("&", And)
    , ("|", Or)
    , ("<", LessThan)
    , (">", GreaterThan)
    , ("=", EqualTo)
    ]

parseExpression :: Parser Expression
parseExpression =
  parseMap2 Expression parseTerm $
    zeroOrMore $ do
      optionalSpaceParser
      op <- parseOp
      optionalSpaceParser
      term <- parseTerm
      return (op, term)

parseLet :: Parser Statement
parseLet = do
  keyword "let"
  requiredSpaceParser
  access <- parseAccess
  optionalSpaceParser
  keyword "="
  optionalSpaceParser
  expression <- parseExpression
  optionalSpaceParser
  keyword ";"
  return (Let access expression)

-- Parses a list of statements, including surrounding whitespace
parseBlock :: Parser [Statement]
parseBlock = do
  optionalSpaceParser
  zeroOrMore $ do
    statement <- parseStatement
    optionalSpaceParser
    return statement

parseConditionAndBlock :: String -> Parser (Expression, [Statement])
parseConditionAndBlock controlKeyword = do
  keyword controlKeyword
  optionalSpaceParser
  keyword "("
  optionalSpaceParser
  expression <- parseExpression
  optionalSpaceParser
  keyword ")"
  optionalSpaceParser
  keyword "{"
  block <- parseBlock
  keyword "}"
  return (expression, block)

parseElse :: Parser [Statement]
parseElse = do
  keyword "else"
  optionalSpaceParser
  keyword "{"
  block <- parseBlock
  keyword "}"
  return block

resolveMaybeList :: Maybe [a] -> [a]
resolveMaybeList Nothing = []
resolveMaybeList (Just as) = as

parseIf :: Parser Statement
parseIf = do
  (expression, block) <- parseConditionAndBlock "if"
  optionalSpaceParser
  elseBlock <- parseMap resolveMaybeList $
    parseOptional parseElse
  return (If expression block elseBlock)

parseWhile :: Parser Statement
parseWhile = do
  (expression, block) <- parseConditionAndBlock "while"
  return (While expression block)

parseDo :: Parser Statement
parseDo = do
  keyword "do"
  requiredSpaceParser
  subCall <- parseSubCall
  optionalSpaceParser
  keyword ";"
  return (Do subCall)

parseReturnStatement :: Parser Statement
parseReturnStatement =
  let
    spaceAndValueParser = do
      requiredSpaceParser
      expression <- parseExpression
      optionalSpaceParser
      return (Just expression)
  in
    do
      keyword "return"
      returnValue <- choice
        [ spaceAndValueParser
        , parseMap (const Nothing) optionalSpaceParser -- this must follow the return value parser since "return" is at the start of "return value"
        ]
      keyword ";"
      return (Return returnValue)

parseStatement :: Parser Statement
parseStatement =
  choice
    [ parseLet
    , parseIf
    , parseWhile
    , parseDo
    , parseReturnStatement
    ]

parseSubroutineType :: Parser SubroutineType
parseSubroutineType =
  parseKeywordValue
    [ ("method", Method)
    , ("constructor", Constructor)
    , ("function", Function)
    ]

parseMaybeVoidType :: Parser (Maybe Type)
parseMaybeVoidType =
  choice
    [ parseKeywordValue [("void", Nothing)]
    , parseMap Just jackTypeParser
    ]

parseParameter :: Parser Parameter
parseParameter = do
  jackType <- jackTypeParser
  requiredSpaceParser
  name <- identifier
  return (Parameter jackType name)

parseSubroutine :: Parser Subroutine
parseSubroutine = do
  methodType <- parseSubroutineType
  requiredSpaceParser
  returnType <- parseMaybeVoidType
  requiredSpaceParser
  name <- identifier
  optionalSpaceParser
  keyword "("
  optionalSpaceParser
  parameters <- optionalParseCommaSeparated parseParameter
  keyword ")"
  optionalSpaceParser
  keyword "{"
  variables <- zeroOrMore $ do
    optionalSpaceParser
    keyword "var"
    requiredSpaceParser
    parseVarDec
  statements <- parseBlock
  keyword "}"
  return (Subroutine methodType returnType name parameters variables statements)

parseClass :: Parser Class
parseClass = do
  optionalSpaceParser -- must include surrounding whitespace because this is the root parser
  keyword "class"
  requiredSpaceParser
  name <- identifier
  optionalSpaceParser
  keyword "{"
  varDecs <- zeroOrMore $ do
    optionalSpaceParser
    parseClassVar
  subroutines <- zeroOrMore $ do
    optionalSpaceParser
    parseSubroutine
  optionalSpaceParser
  keyword "}"
  return (Class name varDecs subroutines)
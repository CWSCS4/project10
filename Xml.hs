module Xml where

import Data.String (unlines)
data Xml
  = XmlNode String [Xml]
  | TextNode String String

indent :: [String] -> [String]
indent = map ("  " ++)

replace :: Char -> String -> String -> String
replace _ _ "" = ""
replace match replacement (c : cs) =
  let
    csReplaced = replace match replacement cs
  in
    if c == match then
      replacement ++ csReplaced
    else
      c : csReplaced

flatten :: [[a]] -> [a]
flatten [] = []
flatten ([] : remaining) = flatten remaining
flatten ((x : xs) : remaining) = x : flatten (xs : remaining)

escapeForText :: String -> String
escapeForText =
  replace '<' "&lt;" .
  replace '>' "&gt;" .
  replace '&' "&amp;"

toLines :: Xml -> [String]
toLines xml =
  case xml of
    XmlNode tag children ->
      ["<" ++ tag ++ ">"] ++
      indent (flatten (map toLines children)) ++
      ["</" ++ tag ++ ">"]
    TextNode tag text ->
      ["<" ++ tag ++ "> " ++ escapeForText text ++ " </" ++ tag ++ ">"]

instance Show Xml where
  show = unlines . toLines

class Xmlable a where
  toXml :: a -> Xml

class XmlArrayable a where
  toXmlArray :: a -> [Xml]
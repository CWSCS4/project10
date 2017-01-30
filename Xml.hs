module Xml where

import Data.List (intercalate)

data Xml
  = XmlNode String [(String, String)] [Xml]
  | TextNode String

indent :: [String] -> [String]
indent = map (\line -> '\t' : line)

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

escape :: String -> String
escape =
  replace '"' "\\\"" .
  replace '"' "\\\\"

flatten :: [[a]] -> [a]
flatten [] = []
flatten ([] : remaining) = flatten remaining
flatten ((x : xs) : remaining) = x : flatten (xs : remaining)

toLines :: Xml -> [String]
tolines (XmlNode tag attributes children) =
  let
    attributeString =
      case attributes of
        [] -> ""
        _ ->
          " " ++
          intercalate " " (map (\(name, value) -> name ++ "=\"" ++ (escape value) ++ "\"") attributes)
  in
    ["<" ++ tag ++ attributeString ++ ">"] ++
    indent (flatten (map toLines children)) ++
    ["</" ++ tag ++ ">"]
toLines (TextNode text) = [text]

instance Show Xml where
  show = intercalate "\n" . toLines
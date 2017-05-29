module Main where

import JackParser (parse, parseClass)
import System.Environment
import System.Exit
import System.IO
import Xml (toXml)

xmlExtension :: String
xmlExtension = ".cs.xml"

failWithMessage :: String -> IO ()
failWithMessage message = do
  hPutStrLn stderr message
  exitFailure

removeEnd :: Eq a => [a] -> [a] -> Maybe [a]
removeEnd ending list =
  if ending == list then
    Just []
  else
    if list == [] then
      Nothing
    else
      let
        (first : remaining) = list
        removeEndRemaining = removeEnd ending remaining
      in
        case removeEndRemaining of
          Nothing -> Nothing
          Just result -> Just (first : result)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      contents <- readFile fileName
      case parse parseClass contents of
        Nothing ->
         failWithMessage ("Could not parse " ++ fileName)
        Just (jackClass, _) ->
          case removeEnd ".jack" fileName of
            Nothing ->
              failWithMessage "File is not a jack file"
            Just bareFileName ->
              let
                xmlString = show (toXml jackClass)
              in
                writeFile (bareFileName ++ xmlExtension) xmlString
    _ ->
     failWithMessage "Syntax: ./Main JackClass.jack"
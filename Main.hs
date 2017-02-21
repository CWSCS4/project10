module Main where

import JackParser

main = do
	contents <- getContents
	parsed <- parse parseClass contents
	putStr (convertXML parsed)
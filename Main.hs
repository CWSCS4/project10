module Main where

import JackParser

main = do
	contents <- getContents
	parsed <- parse parseClass contents
	print (convertXML parsed)
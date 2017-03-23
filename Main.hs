module Main where

import JackParser

main :: IO ()
main = do
	contents <- getContents
	let parsed = parse parseClass contents
	putStr (convertXML parsed)

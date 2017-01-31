#!/bin/bash

ghc -Wall -XFlexibleInstances Main.hs -o Main || exit

for jackFile in */*.jack; do
	outFile=$jackFile.cs.xml
	./Main $jackFile > $outFile
	correctXmlFile=${jackFile%jack}xml
	diff $correctXmlFile $outFile
done
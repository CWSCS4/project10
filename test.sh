#!/bin/bash

stack ghc -- -Wall -XFlexibleInstances Main.hs -o Main || exit

for jackFile in */*.jack; do
	bareFile=${jackFile%.jack}
	./Main $jackFile
	correctXmlFile=$bareFile.xml
	outFile=$bareFile.cs.xml
	diff --strip-trailing-cr $correctXmlFile $outFile
done
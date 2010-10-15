all: markup testmarkup
.PHONY: all

markup: Main.hs Markup.hs XML.hs
	ghc --make Main.hs Markup.hs -o markup

testmarkup: Tests.hs Markup.hs
	ghc --make Tests.hs Markup.hs -o testmarkup

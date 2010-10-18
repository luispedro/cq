all: markup testmarkup
.PHONY: all

markup: Main.hs Markup.hs Parse.hs
	ghc --make -O Main.hs Markup.hs -o markup

testmarkup: Tests.hs Markup.hs Parse.hs
	ghc --make -O Tests.hs Markup.hs -o testmarkup

presentation: DepTypes.tex
	latexmk -pdf DepTypes.tex
  
DepTypes.tex: *.hs
	runhaskell DepTypes > DepTypes.tex

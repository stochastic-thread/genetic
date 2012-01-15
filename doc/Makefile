all: main.pdf

main.pdf: main.tex contents.tex
	xelatex -interaction=nonstopmode -shell-escape main.tex
	xelatex -interaction=nonstopmode -shell-escape main.tex
	xelatex -interaction=nonstopmode -shell-escape main.tex

clean:
	rm -f *.aux *.log *~ .*~ *.pyg *.out *.toc

cleanall: clean
	rm -f *.pdf

.PHONY: clean cleanall


.SUFFIXES: .lhs .mkd .html .tex .pdf

PANDOC := pandoc --no-wrap -sS -f markdown+lhs
PANDOCH := pandoc --no-wrap -sS
HSCOLOUR := hscolour -lit

.lhs.mkd:
	cat $< | $(HSCOLOUR) -css > $@

.lhs.html:
	cat $< | $(HSCOLOUR) -css | $(PANDOCH) -t html -c data/hscolour.css > $@

.lhs.tex:
	cat $< | $(PANDOC) -t latex> $@

#.lhs.tex:
#	cat $< | $(HSCOLOUR) -latex | $(PANDOC) -t latex> $@

.tex.pdf:
	pdflatex $< && pdflatex $< && pdflatex $<

# Run e. g. make Main.pdf to get the latter file from a Main.lhs file...
# http://passingcuriosity.com/2008/literate-haskell-with-markdown-syntax-hightlighting/

SOURCES = implementation.tex
TARGETS = $(patsubst %.tex, %.pdf, ${SOURCES})

DIA_IN  = $(shell find -name "*.dot")
DIA_OUT = $(DIA_IN:%.dot=%.eps)


${TARGETS} : $(DIA_OUT)
${TARGETS} : %.pdf : %.tex
	@pdflatex ${*}.tex
	@biber    ${*}
	@pdflatex ${*}.tex
	@biber    ${*}
	@pdflatex ${*}.tex

all      : ${TARGETS}

diagrams: $(DIA_OUT)
diagrams/%.eps : diagrams/%.dot
	dot -Tps $< > $@

clean    :
	@rm -f *.aux *.bbl *.bcf *.blg *.log *.out *.pdf *.version *.xml *.toc *.bak
spotless : clean
	@rm -f *.pdf 

pdflatex=pdflatex

introduction.tex = \
	introduction/introduction.tex \
    	introduction/preamble.tex \
	introduction/molecular_dynamics.tex \
	introduction/objectives.tex

report.tex = report.tex macros.tex $(introduction.tex)

report.pdf: $(report.tex)
	$(pdflatex) report.tex

.PHONY: clean
clean:
	@echo rm *.aux *.log *.pdf
	@rm *.aux *.pdf *.log 2>/dev/null; true

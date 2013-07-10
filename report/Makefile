pdflatex=./latexmk.pl -pdf
subdirs = introduction background methodology


introduction.tex = \
	introduction/introduction.tex \
    	introduction/preamble.tex

background.tex = \
	background/background.tex \
	background/molecular_dynamics.tex \
	background/lennard_jones_potential.plt.tex \
	background/equations_of_motion.tex

methodology.tex = \
	methodology/methodology.tex \
	methodology/preamble.tex

report.tex = \
	report.tex \
	macros.tex \
	$(introduction.tex) \
	$(background.tex) \
	$(methodology.tex)


# Compile report
report.pdf: $(report.tex)
	$(pdflatex) report.tex


# Compile plots into latex includable format
%.plt.tex: %.plt.gnu
#	# Compile .plt.eps and .plt.tex files
	gnuplot $<
#	# Compile .plt.pdf file
	epstopdf $*.plt.eps
#	# Correct .plt.tex file to allow .plt.pdf file to be referenced as .plt
	sed -i 's#{$*.plt}#[type=pdf,ext=.pdf,read=.pdf]{$*.plt}#' $@

# Clean up
.PHONY: clean
clean_exts = .aux .log .pdf .plt.eps .plt.tex
clean_all=$(foreach dir, $(subdirs), $(addprefix $(dir)/*, $(clean_exts)))
clean:
	@echo rm $(clean_all)
	@rm $(clean_all) 2>/dev/null; true

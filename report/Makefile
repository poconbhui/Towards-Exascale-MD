pdflatex=./latexmk.pl -pdf -g
subdirs = introduction background methodology logos


introduction.tex = \
	introduction/introduction.tex \
    	introduction/preamble.tex

background.tex = \
	background/background.tex \
	background/molecular_dynamics.tex \
	background/lennard_jones_potential.plt.tex \
	background/parallel_schemes.tex

methodology.tex = \
	methodology/methodology.tex \
	methodology/preamble.tex

parallel_implementation.tex = \
        parallel_implementation/parallel_implementation.tex \
        parallel_implementation/preamble.tex \
        parallel_implementation/v0/replicated.individual_operation.512.logspeedup.plt.tex \
        parallel_implementation/v0/replicated.pair_operation.512.logspeedup.plt.tex


report.tex = \
	report.tex \
	epcc.sty \
	macros.tex \
	$(introduction.tex) \
	$(background.tex) \
	$(methodology.tex) \
        $(parallel_implementation.tex)


# Compile report
report.pdf: $(report.tex)
	$(pdflatex) report.tex


# Compile plots into latex includable format
%.plt.tex: %.plt.pdf %.plt.gnu
#	# Correct .plt.tex file to allow .plt.pdf file to be referenced as .plt
	sed -i 's#\\includegraphics{#\\includegraphics[type=pdf,ext=.pdf,read=.pdf]{#' $@

%.plt.eps: %.plt.gnu
#	# Compile .plt.eps and .plt.tex files
	gnuplot $*.plt.gnu

%.plt.pdf: %.plt.eps
#	# Compile .plt.pdf file
	epstopdf $*.plt.eps


# Compile bash plots into gnuplots
%.plt.gnu: %.plt.sh
	bash $<


logos_pdf = $(patsubst %.eps, %.pdf, $(wildcard logos/*.eps))
epcc.sty : $(logos_pdf)

# Compile eps images to pdf
%.pdf : %.eps
	epstopdf $<

# Clean up
.PHONY: clean
clean_exts = .aux .log .pdf .plt.eps .plt.tex
clean_all=$(foreach dir, $(subdirs), $(addprefix $(dir)/*, $(clean_exts)))
clean:
	@echo rm $(clean_all)
	@rm $(clean_all) 2>/dev/null; true

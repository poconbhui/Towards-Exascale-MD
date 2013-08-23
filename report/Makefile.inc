#
# Makefile.inc
#
# Several useful rules here for inclusion in Makefiles throughout the
# report directory.
#

# Set up pdflatex command
pdflatex=./latexmk.pl -pdf $(LATEX_FLAGS)\
    -latexoption=-file-line-error \
    -latexoption=-halt-on-error \
    -latexoption=-interaction=nonstopmode


# Compile plots into latex includable format
.PRECIOUS: %.plt.pdf

%.plt.tex: %.plt.pdf %.plt.gnu
#	# Compile .plt.eps and .plt.tex files
	gnuplot $*.plt.gnu
#
#	# Correct .plt.tex file to allow .plt.pdf file to be referenced as .plt
	sed -i 's#\\includegraphics{#\\includegraphics[type=pdf,ext=.pdf,read=.pdf]{#' $@

%.plt.eps: %.plt.gnu
#	# Compile .plt.eps and .plt.tex files
	gnuplot $*.plt.gnu


# Compile bash plots into gnuplots
%.plt.gnu: %.plt.sh
	bash $<

# Compile eps images to pdf
%.pdf : %.eps
	epstopdf $<

# Clean up
.PHONY: clean
clean_exts = .aux .log .toc .lof .lot .fdb_latexmk .fls .pdf .plt.eps .plt.tex .plt.pdf

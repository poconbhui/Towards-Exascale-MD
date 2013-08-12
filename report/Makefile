pdflatex=./latexmk.pl -pdf -g \
    -latexoption=-file-line-error \
    -latexoption=-halt-on-error \
    -latexoption=-interaction=nonstopmode

subdirs = introduction background methodology logos


introduction.tex = \
	introduction/introduction.tex \
    	introduction/preamble.tex

background.tex = \
	background/background.tex \
	background/molecular_dynamics.tex \
	background/lennard_jones_potential.plt.tex \
	background/lennard_jones_potential.plt.pdf \
	background/parallel_schemes.tex

methodology.tex = \
	methodology/methodology.tex \
	methodology/preamble.tex

parallel_implementation_graphs_sources = \
    $(wildcard parallel_implementation/v0/*.plt.sh)
parallel_implementation_graphs = \
    $(patsubst %.sh, %.tex, $(parallel_implementation_graphs_sources)) \
    $(patsubst %.sh, %.pdf, $(parallel_implementation_graphs_sources))
parallel_implementation.tex = \
        parallel_implementation/parallel_implementation.tex \
        parallel_implementation/preamble.tex \
        parallel_implementation/replicated_data.tex \
        parallel_implementation/systolic_loop.tex \
        \
        $(parallel_implementation_graphs)


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


logos_pdf = $(patsubst %.eps, %.pdf, $(wildcard logos/*.eps))
epcc.sty : $(logos_pdf)

# Compile eps images to pdf
%.pdf : %.eps
	epstopdf $<

# Clean up
.PHONY: clean
clean_exts = .aux .log .toc .fdb_latexmk .fls .pdf .plt.eps .plt.tex .plt.pdf
clean_all=$(foreach dir, . $(subdirs), $(addprefix $(dir)/*, $(clean_exts)))
clean:
	@echo rm $(clean_all)
	@rm $(clean_all) 2>/dev/null; true

include Makefile.inc

#
# Makefile
#
# Compile report.pdf
#


subdirs = introduction background methodology logos parallel_implementation
make_subdirs = parallel_implementation


#
# Compile subdirs and the report
#
all: $(make_subdirs) report.pdf



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
	methodology/preamble.tex \
	methodology/implementation.tex \
	methodology/performance_measurement.tex


parallel_implementation.tex = \
	parallel_implementation/parallel_implementation.tex

conclusion.tex = \
	conclusion/conclusion.tex \
	conclusion/preamble.tex



report.tex = \
	report.tex \
	epcc.sty \
	macros.tex \
	packages.tex \
	styles.tex \
	$(introduction.tex) \
	$(background.tex) \
	$(methodology.tex) \
        $(parallel_implementation.tex) \
	$(conclusion.tex)



# Compile report
report.pdf: $(report.tex)
	$(pdflatex) report.tex


# Compile data in subdirs
.PHONY: $(make_subdirs)
$(make_subdirs):
	make -C $@ all



logos_pdf = $(patsubst %.eps, %.pdf, $(wildcard logos/*.eps))
epcc.sty : $(logos_pdf)


# Clean up
.PHONY: clean
clean_all=$(foreach dir, . $(subdirs), $(addprefix $(dir)/*, $(clean_exts)))
make_clean_all=\
	$(foreach dir, $(make_subdirs), make -C $(dir) clean; )
clean:
	$(make_clean_all)
	@echo rm $(clean_all)
	@rm $(clean_all) 2>/dev/null; true

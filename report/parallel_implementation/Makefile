include ../Makefile.inc

all: parallel_implementation.tex


graphs_sources = \
    $(wildcard v0/*.plt.sh) \
    $(wildcard v1/*.plt.sh) \
    log_vs_log_sqrt_p.plt.gnu \
    replicated_systolic_comm_fits.plt.gnu
graphs = \
    $(patsubst %.sh, %.tex, $(graphs_sources)) \
    $(patsubst %.sh, %.pdf, $(graphs_sources)) \
    $(patsubst %.gnu, %.tex, $(graphs_sources)) \
    $(patsubst %.gnu, %.pdf, $(graphs_sources))

parallel_implementation.tex = \
        parallel_implementation.tex \
        preamble.tex \
        replicated_data.tex \
        systolic_loop.tex \
	shared_and_replicated_data.tex \
	replicated_systolic_loop.tex \
        \
        $(graphs)

parallel_implementation.tex: $(parallel_implementation.tex)
	touch parallel_implementation.tex


.PHONY: clean
clean:
	@echo rm {v0,v1}/*.plt.{tex,pdf,eps,gnu}
	@rm {v0,v1}/*.plt.{tex,pdf,eps,gnu} 2>/dev/null; true
	@echo rm ./*.plt.{tex,pdf,eps}
	@rm ./*.plt.{tex,pdf,eps} 2>/dev/null; true

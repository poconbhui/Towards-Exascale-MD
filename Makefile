#
# Before using Makefile, be sure you've run configure.rb to
# configure the project for your system.
#
# For help, run ./configure.rb --help
#

# Define subdirectories containing programs to be compiled.
SUBDIRS=src test bench

#
# Target all compiled all programs in all SUBDIRS using target all.
#
.PHONY: all
all: $(SUBDIRS)


.PHONY: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@ all

#
# Run unit tests
#
.PHONY: run_test
run_test: test
	./scripts/run_tests.rb

#
# Run make clean in all SUBDIRS.
#
clean:
	$(foreach DIR, $(SUBDIRS), $(MAKE) -C $(DIR) clean;)

#
# Run clean. Remove compiled templates
#
distclean: clean
	rm scripts/exec.sh
	rm scripts/Makefile.inc

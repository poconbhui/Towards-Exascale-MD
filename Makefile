#
# Before using Makefile, be sure you've set environment variables in
# config.sh to match the mpi compiler and executor on your system.
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
run_test:
	cd test && ../config.sh ./run_tests.rb

#
# Run make clean in all SUBDIRS.
#
clean:
	$(foreach DIR, $(SUBDIRS), $(MAKE) -C $(DIR) clean;)

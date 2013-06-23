SUBDIRS=src test bench

#
# Make targets
#
.PHONY: all
all: src

.PHONY: $(SUBDIRS)
$(SUBDIRS):
	make -C $@ all

.PHONY: test
run_test:
	cd test && ../config.sh ./run_tests.rb

clean:
	$(foreach DIR, $(SUBDIRS), make -C $(DIR) clean;)

SUBDIRS=src test

#
# Make targets
#
.PHONY: all
all:
	make -C src all


.PHONY: test
test:
	make -C test all

clean:
	$(foreach DIR, $(SUBDIRS), make -C $(DIR) clean;)

# COMP 321 Homework 1:  Practice with functors.
#
# N. Danner

# Dependency lists.

TARGETS=tests

tests_DEPS=tests.cm tests.sml unit_test.sml pri_queue.cm listpq.sml pri_key.sig

# SML/NJ programs

SML_ROOT=/opt/smlnj
SML=$(SML_ROOT)/bin/sml
ML_BUILD=$(SML_ROOT)/bin/ml-build
H2E=/usr/local/bin/heap2exec32
ANO=$(SML_ROOT)/bin/.arch-n-opsys

# ##########
# It should not be necessary to modify anything below this line.
# ##########

# Options and additional CM files for ml-build.
ML_BUILD_OPTS=-Ctdp.instrument=true
ML_BUILD_CMS=\$$smlnj-tdp/back-trace.cm

# Compute the heap suffix.
HEAP_SUFFIX=$(shell $(SML) @SMLsuffix)

# Capitalize the first letter of the first argument.
capitalize = $(shell echo $(1) | cut --characters 1 | tr [:lower:] [:upper:])$(shell echo $(1) | cut --characters 1 --complement)

# Targets

# Each target is built from the corresponding heap image using heap2exec.

tests : % : %.$(HEAP_SUFFIX)
	$(H2E) $@.$(HEAP_SUFFIX) $@

# Each heap image is built using ml-build from the prerequisite list that
# corresponds to the heap image basename.

.SECONDEXPANSION:
%.$(HEAP_SUFFIX) : $$($$*_DEPS)
	$(ML_BUILD) $(ML_BUILD_OPTS) $(ML_BUILD_CMS) \
	$*.cm $(call capitalize,$*).main $@

# Cleanup targets.
clean :
	rm -f *.lex.sml *.grm.sml
	rm -f $(TARGETS)
	rm -f $(addsuffix .$(HEAP_SUFFIX), $(TARGETS))

realclean : clean
	rm -rf .cm

mftest :
	@echo tests_DEPS = $(tests_DEPS)

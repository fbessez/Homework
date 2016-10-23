# COMP 321 Homework 2:  ML-ulex specification.
#
# N. Danner

# Dependency lists.

TARGETS=nums

nums_DEPS=nums.cm nums.lex nums.sml tokens.sml

# SML/NJ programs

SML_BIN=
SML=$(SML_BIN)sml
ML_BUILD=$(SML_BIN)ml-build
H2E=/usr/local/bin/heap2exec32

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

nums : % : %.$(HEAP_SUFFIX)
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


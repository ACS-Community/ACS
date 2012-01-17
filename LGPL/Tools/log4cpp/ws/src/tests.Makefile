# Makefile.tests
# On 'make all' the tests are not built.
# 'make check' does building and executing the tests at the same time.
# Rules for building the tests only does not exist.
# For this, make has to be invoked with each test name individually.
# The list of tests is in log4cpp-$(LOG4CPP_VER)/tests/Makefile.am
# But this file is not extracted at time of initial 'make all'.
# Therefore delegate inclusion and building to a recursively called makefile,
# after completion of patch or configure.
# To avoid confusion don't call the original Makefile nested, and name it differently.
#
# Get the current version passed in as makefile variable or include the common Makefile.
#
# Variables from tests/Malefile.am, in particular:
# TESTS = Individual test applications
# check_DATA = Files needed  to execute the test applications
# noinst_PROGRAMS = manual test applications
# all not covered by 'make install'
#
# Note: Executing the test applications is covered by 'make check', a superset of 'make all'.
#  Cleaning up the tet applications by 'make clean'
#  Build done when necessary before running the tests.

# Suffix of executable applicaions
ifeq ($(OSYSTEM),$(CYGWIN_VER))
EXEEXT =.exe
else
EXEEXT =
endif

# For version number LOG4CPP_VER
include common.Makefile

# Make variables with filenames pf testcases 
include log4cpp-$(LOG4CPP_VER)/tests/Makefile.am

# Building the test applications
all :
	$(MAKE) -C log4cpp-$(LOG4CPP_VER)/tests $(addsuffix $(EXEEXT),$(TESTS))
	  
# Installing the test applications
install :
	cp -pt $(INSTALL_DIR)/bin $(addprefix log4cpp-$(LOG4CPP_VER)/tests/,$(addsuffix $(EXEEXT),$(TESTS)) $(addsuffix $(EXEEXT),$(noinst_PROGRAMS)) $(check_DATA))

#___oOo___

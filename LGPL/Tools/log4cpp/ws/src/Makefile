#*******************************************************************************
#
# "@(#) $Id: Makefile,v 1.7 2011/02/11 17:33:36 htischer Exp $"
#
# Makefile of log4cpp
#
# who       when      what
# --------  --------  ----------------------------------------------
# javarias  05/05/10  created
# htischer  21/01/11  build output more consistent and self descriptive
#					  Create new patch version including sourcecode updates
#					  on 'make clean_dist'
#					  On unpack/patch/clean do not overwrite sourcecode updates
#					  Allow build with 'make all' and allow make 'clean'
#					    without reconfiguring and without overwriting source
#					    modifications done after unpacking.
# htischer  25/01/11  Move ws/lcu common part to a common file (version numbers).
#					  Remove editor backup files *~. Comments.
# htischer  28/01/11  clean_configure to clean up without creating a new patch
#					  exclude autom4te.cache from patch.
# htischer  31/01/11  moved INSTALL_DIR to common file
#					  add build, install, exec of testcases
# rtobar    02/02/11  on 'make clean', internally also clean the configure results of log4cpp
# htischer  11/02/11  Rename 'make clean' without rtobar change to 'make clean_build'
#					  Rename 'make clean_configure' to 'make clean'
#                     Touch the patched configure to making sure that it is
#					  newer as the dependents and rebuilding scripts is not attempted.
#					  Enhance testsuite, test log with preprocess for easier diff

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
# make targets:
#
# all			Build. Do unpack, patch and configure if and only if necessary.
#
# clean_dist	Bring subtree in a state suitable for version control checkin.
#				This includes also creating a new patch version in case of
#				modifications in the extracted sourcecode.
#
# clean         remove configure results, if they got created.
#				Avoids doing a configure just to be able to do a distclean.
#				Useful to start configure from scratch or checking current state
#				before creating a new patch. 
#
# clean_build	Prepares for a clean rebuild, but leaves configure results.
#
# unpack		Unpack the original log4cpp package.
#
# patch			patch the original log4cpp package with sourcecode updates.
#				unpack when necessary.
#
# configure		autoconfigure the build to the current runtime/build environment.
#				Unpack and patch when necessary.
#
# test			Run a subset of the provided test code.
#
# printenv		Dump the Shell environment as seen inside make
#------------------------------------------------------------------------

# Settings to be synchronized in ../../ws/src/Makefile and ../../lcu/src/Makefile
# Moved to a common file for inclusion 

# Version of current patched state of log4cpp package
# Version of original tgz of log4cpp package
# LOG4CPP_VER = 1.0+
# LOG4CPP_TAR = 1.0

# Installation location for 'make install'
# INSTALL_DIR = e.g. $(INTROOT)

include common.Makefile

#
# Scripts (public and local)
# ----------------------------
SCRIPTS_L       =

#>>>>> END OF standard rules

#
# INCLUDE STANDARDS
# -----------------

#MAKEDIRTMP := $(shell searchFile include/acsMakefile)
#ifneq ($(MAKEDIRTMP),\#error\#)
#   MAKEDIR := $(MAKEDIRTMP)/include
#   include $(MAKEDIR)/acsMakefile
#endif

#
# TARGETS
# -------

# patch and configure are not repeated on calling again, therefore no separate build rule needed.
# Build the testcases in addition to the library and test driver.
# 'make check' builds the testcases as well as executes them.
# Therefore do the build as separate step. Not available as separate pseudotarget.
# List of testcases in log4cpp-$(LOG4CPP_VER)/tests/makefile.am not available before
# unpacking and patching. Therefore delegate into sub-make to be invoked after this.
# Need to treat 'make install' and 'make tests' special also. But 'make clean_build' already cleans the tests.
.PHONY : all
all: configure
	@echo ". . . building . . ."
	$(MAKE) -C log4cpp-$(LOG4CPP_VER) all
	$(MAKE) -f tests.Makefile all
	@echo " . . . 'all' done" 

# Clean only the build results, but keep the configure results.
# For cleaning the configure results also, use 'make clean'.
# For packaging everything ready for version control use 'make clean_dist'.

.PHONY : clean_build 
clean_build : 
	@if [ -r log4cpp-$(LOG4CPP_VER)/Makefile ]; then \
		echo $(MAKE) -C log4cpp-$(LOG4CPP_VER) clean; \
		$(MAKE) -C log4cpp-$(LOG4CPP_VER) clean; \
	fi
	@echo " . . . clean_build done"

# Remove results of configure
# Useful to rerun configure from scratch, or to check current state result before creating a new patch version
# Can be done only if Makefile got created during configuration
# Use Makefile existence as indicator.
# Cannot use make dependency, to avoid creating files which immediately get deleted again.
# Attention!
# If a configure run breaks before creating the Makefile, the file tree is inconsistent!
# In this case don't create a new patch untile configure was runing successfully!

.PHONY : clean
clean :
	@echo ". . . clean . . ."
	@if [ -r log4cpp-$(LOG4CPP_VER)/Makefile ]; then \
		echo $(MAKE) -C log4cpp-$(LOG4CPP_VER) distclean; \
		$(MAKE) -C log4cpp-$(LOG4CPP_VER) distclean; \
	fi
	@echo " . . . clean done"

# Create a new patch version to reflect changes in the sourcecode.
# Before that clean up the build of the package to the state of distribution (nested distclean)
# Attention!
# If configure failed previously, clean or distclean cannot clean up appropriately!
# Create a new patch version only if make configure was successful!
# No need to do a clean of the package, because done in turn of distclean.
# After creating a new patch, to be ready for checkin, rm also unpacked and patched subtrees
# Attention!
# Do the rm of the modified source only if the new patch version was successfully created!
# (but empty patch on identical is ok)
# After checkin do "make all" or "make patch" to restore the extracted modified sources. 
#
# Creating a new patch works only and is only necessary if the patched folder is existing.
# To avoid unnecessary recreation of a new patch version end extracted original version on
# repeated run of clean_dist, do not use make dependencies, but check at runtime with shell.
# 'diff a b >redirection': exit codes '$?' ambiguous:
# 0 = input identical
# 1 = differences found
# 1 = redirection failed
# 1 = on -Naur, one of the input arguments did not exist
# 2 = without -Naur, one of the input arguments did not exist
# 2 = with -Naur, no one of the input arguments did exist.
# Therefore
# - make sure that both input argument folders to diff exist (verify result folder of tar -x)
# - rm old tmp version of patch, only if it exists. (rm -f still has exit code if file survives e.g. r/o folder)
# - ignore diff exit code
# - check if tmp version of new patch now exists
# - only on success rm old version of patch and replace by tmp version
#
# Notes:
# echo commands to see reason of actions of hidden if.
# Need to escape ">" redirection char in echo of command.
# $ which should be processed by sh not by make needs to be escaped by $$
# For the log4cpp package clean up for archive is called "distclean", not "clean_dist".
# on all commands except last, exit shell script on error to avoid working on corrupted data by 'command || exit 1'
#
# Todo:
# A new patch is only needed if a file was deleted, or a file is newer as the tgz.
# But on no change and keeping file dates on copy the new patch should be identical or equivalent,
# therefore for now skip this complicated check.
#
# Attention!
# According to current observations, autom4te.cache contained only empty files. Empty files do not go into patch.
# But danger if the wrong autom4te.cache makes it to the lcu side which runs with a different configuration.
# autom4te.cache: Can be savely deleted. Created on certain configure steps. Turn off creation with one of
# autom4te.cfg ~/.autom4te.cfg ./.autom4te.cfg :
# begin-language: "Autoconf-without-aclocal-m4"
# args: --no-cache
# end-language: "Autoconf-without-aclocal-m4"
#
# Note:
# If build breaks after 'make -C log4cpp distclean' but before 'rm -r distclean', then manual investigation
# is needed, because 'make patch' is rejected and 'make clean_dist' cannot be resumed half way through.
# - Either manually run configure with pseudo target 'configure'
# - Or manually preserve all modified source files, remove patched folder,
#   restore 'make patch' and manually preserved files.
.PHONY : clean_dist 
clean_dist : clean
	@echo ". . . clean_dist . . ."
	rm -f `find .. -name "*~"`
	@if [ -d log4cpp-$(LOG4CPP_VER) ]; then \
		echo rm -rf log4cpp-$(LOG4CPP_VER)/autom4te.cache; \
		rm -rf log4cpp-$(LOG4CPP_VER)/autom4te.cache || exit 1; \
		if [ ! -d log4cpp-$(LOG4CPP_TAR) ]; then \
			echo tar -xzf log4cpp-$(LOG4CPP_TAR).tar.gz; \
			tar -xzf log4cpp-$(LOG4CPP_TAR).tar.gz || exit 1; \
			[ -d log4cpp-$(LOG4CPP_TAR) ] || exit 1; \
		fi; \
		echo rm -f log4cpp-$(LOG4CPP_VER).patch.tmp; \
		rm -f log4cpp-$(LOG4CPP_VER).patch.tmp || exit 1; \
		echo diff -Naur log4cpp-$(LOG4CPP_TAR) log4cpp-$(LOG4CPP_VER) \>log4cpp-$(LOG4CPP_VER).patch.tmp; \
		diff -Naur log4cpp-$(LOG4CPP_TAR) log4cpp-$(LOG4CPP_VER) >log4cpp-$(LOG4CPP_VER).patch.tmp; \
		[ -r log4cpp-$(LOG4CPP_VER).patch.tmp ] || exit 1; \
		echo mv log4cpp-$(LOG4CPP_VER).patch.tmp log4cpp-$(LOG4CPP_VER).patch; \
		mv -f log4cpp-$(LOG4CPP_VER).patch.tmp log4cpp-$(LOG4CPP_VER).patch; \
	fi
	rm -rf log4cpp-$(LOG4CPP_TAR) log4cpp-$(LOG4CPP_VER) configure.log log4cpp_testmain.log log4cpp_teststimes.log log4cpp_tests.log
	@echo " . . . clean_dist done"

.PHONY : man
man :
	$(MAKE) -C log4cpp-$(LOG4CPP_VER)/doc all install
	@echo " . . . man page(s) done"

# Do Nothing, for compatibility
.PHONY: db
db :
	@echo " . . . ../DB done"

# Install the testcases also, see rule 'all'.
.PHONY : install 
install : 
	@echo ". . . installing log4cpp . . ."
	$(MAKE) -C log4cpp-$(LOG4CPP_VER) install
	$(MAKE) -f tests.Makefile install
	@echo " . . . installation done"

# Folder name of extracted tgz. Create only if not existing. No dependencies.
# tar is identical to gtar.
# Attention!
# If failing after folder creation but before completion it is not repeated!
# In this case rm folder manually!
log4cpp-$(LOG4CPP_TAR) :
	@echo ". . . unpacking the tar files . . ."
	tar -xzf $@.tar.gz
	@echo " . . . unpacking the tar files done"
	
# pseudo target name is an alias of the unpacked folder name. Create as dependency
.PHONY : unpack
unpack : log4cpp-$(LOG4CPP_TAR)

# Folder name of patched version of extracted tgz. Create only if not existing, therefore no dependencies.
# Therefore put the dependency to creation of the original folder before each invocation of the patch.
# On target creation do not delete files which appear as targets.
# Therefore recursive copy of original version, instead of mv. -p to keep date of patch creation.
# Anyway copy of original needed later for creating a new patch.
# Do the patch at same time as the folder creation to leave no doubt whether it is done, because
# this cannot be repeated without overwriting sourcecode changes.
# Attention!
# If patch is failing after folder creation but before completion it is not repeated!
# Remove folder manually!
#
# Attention:
# unnecessary execution of autoconf and automake or warning messages
# 'missing --run automake-1.10', 'missing --run autoconf' etc
# This means that a dependent file has a newer timestamp after unpacking and patching from the archive.
# Non available autoconf tool is simulated by simply touching the dependent files.
# See ./config/missing actions, and $(AUTOCONF) use in top Makfile to see when it is executed.
# In particular $(top_srcdir)/m4/ACX_PTHREAD.m4 and configure got patched consistently.
# configure must be newer.
#

log4cpp-$(LOG4CPP_VER) :
	@echo ". . . patching . . ."
	cp -Rp log4cpp-$(LOG4CPP_TAR) $@
	cd $@; patch -p1 < ../log4cpp-$(LOG4CPP_VER).patch
	sleep 2
	touch log4cpp-$(LOG4CPP_VER)/configure
	@echo " . . . patch applied"

# pseudo target name is an alias of the patched folder name. Create as dependency.
# unpack and cannot be a dependency of log4cpp-$(LOG4CPP_VER) but required, therefore
# prepend here as dependency.
# Use .NOTPARALLEL to avoid need of artifical dependencies for build order of dependencies on same line.
.NOTPARALLEL :
.PHONY : patch 
patch : unpack log4cpp-$(LOG4CPP_VER)

# File name of one configure result. Created by configure.
# Create only if not existing, therefore no dependencies.
# Attention!
# If configure fails half way through, the file tree is not consistent, but distclean
# also does not work!
# Do not create a new patch version until configure was successful!
log4cpp-$(LOG4CPP_VER)/Makefile :
	@echo ". . . running configure . . ."
	echo "   log4cpp tar file is: log4cpp-$(LOG4CPP_TAR).tar.gz" >configure.log
	echo "   log4cpp version  is: $(LOG4CPP_VER)" >>configure.log
	cd log4cpp-$(LOG4CPP_VER); ./configure --enable-shared --prefix=$(INSTALL_DIR) >>../configure.log 2>&1
	@echo " . . . configuration file created"

# Do the configure only once if not done now. Use Makefile existence as indicator.
# Separate rule from patch allows to observe the patched result without configure additions,
# and also allows repeating configure in case of failed clean_dist.
# Use .NOTPARALLEL to avoid need of artifical dependencies for build order of dependencies on same line.
.NOTPARALLEL :
.PHONY : configure
configure : patch log4cpp-$(LOG4CPP_VER)/Makefile

# Debugging

.PHONY : printenv 
printenv:
	env | sort

# Run the predefined testsuite and extra tests of noinst_PROGRAMS
#
# testmain [-n <count>] [<filename>]
#  once per second write into syslog (/var/log/messages) if available, otherwise to stdout.
#  -n : iterations, default 10000. After 10 iterations reopen the log file.
#  filename: additional log, default stdout
#  (using neither syslog nor filename, results in two times logging to stdout)
# testbench [<count> [<size>]]
# Benchmark with timing.
# count: records per iteration, default=100
# size: size per record, default=128
# Environment variable CLOCK_USE_CPU to use CPU timestamp instead of posix clock (not calibrated to usec!)
#
# 'make check' runs a testsuite defined by the package. Also builds, if not already done.
# testConfig and testPropertyConfig need additional files in current folder, see make install
# testbench needs redirection of stderr to have output in order
# redirect test output into file
# For easier diff of results remove time stamps
# log4cpp_tests.log
# testmain appends to the log, therefore delete log4cpp_testmain.log before test to get consistent result.
.PHONY : tests 	
tests :
	@echo "{...tests...} 2>&1 | tee log4cpp_teststimes.log"
	@{ \
	echo $(MAKE) -C log4cpp-$(LOG4CPP_VER) check; \
	$(MAKE) -C log4cpp-$(LOG4CPP_VER) check; \
	echo log4cpp-$(LOG4CPP_VER)/tests/testmain -n 21; \
	log4cpp-$(LOG4CPP_VER)/tests/testmain -n 21; \
	echo rm -f log4cpp_testmain.log; \
	rm -f log4cpp_testmain.log; \
	echo log4cpp-$(LOG4CPP_VER)/tests/testmain -n 21 log4cpp_testmain.log; \
	log4cpp-$(LOG4CPP_VER)/tests/testmain -n 21 log4cpp_testmain.log; \
	echo cat log4cpp_testmain.log; \
	cat log4cpp_testmain.log; \
	echo log4cpp-$(LOG4CPP_VER)/tests/testbench 10 5; \
	log4cpp-$(LOG4CPP_VER)/tests/testbench 10 5; \
	} 2>&1 | tee log4cpp_teststimes.log
	sed \
	-e "s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]/XXXX-XX-XX/" \
	-e "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9],[0-9][0-9][0-9]/XX:XX:XX,XXX/" \
	-e "s/[0-9][0-9] [A-Z][A-Za-z][A-Za-z] [0-9][0-9][0-9][0-9]/XX XXX XXXX/" \
	-e "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]/XX:XX:XX.XXX/" \
	-e "s/[0-9][0-9]*\.*[0-9]* us/XX.XX us/" \
	-e "s/^[0-9][0-9][0-9][0-9]*/XXX/" \
	log4cpp_teststimes.log > log4cpp_tests.log
	@echo " . . . tests done"

#___oOo___

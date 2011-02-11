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
EXEEXT =

# For version number LOG4CPP_VER
include ../../ws/src/common.Makefile

# Make variables with filenames pf testcases 
include log4cpp-$(LOG4CPP_VER)/tests/Makefile.am

# Compiling the test applications
# The link by configure is not useful for VxWorks. Need to do it as extra step.
# So far put all application code into a single library for reuse by caller.
# This needs to cover also noinst_PROGRAMS (testmain, testbench), which got compiled already by 
# $(MAKE) -C log4cpp-$(LOG4CPP_VER)/tests all
# By callers makefile 'export' tool settings got inherited down 
# Before 'ar qc' delete old lib to not keep old modules at beginning of files on appending.
# Module Clock by configure gets linked to an application.
# This is not done automatically on creating a library, therefore add explicitely.
# Also create an expect script to execute all tests remotely
# Note: currently the testbench test hangs at last line, therefore wait for last output instead of shell prompt.
# host and execution folder need to be given as arguments to the script
.PHONY : all 
all : expect
	$(MAKE) -C log4cpp-$(LOG4CPP_VER)/tests $(addsuffix $(EXEEXT),$(TESTS))
	rm -f ../lib/libtests.a
	cd log4cpp-$(LOG4CPP_VER)/tests; $(AR) qc ../../../lib/libtests.a $(addsuffix .o,$(TESTS) Clock $(noinst_PROGRAMS))

# Create script to rlogin into remote VxWorks host and executing all individual tests.
# Usage of script:
# rtests.expect <RemoteHost> <RemoteBinFolder> <RemoteTmpFolder>
# RemoteHost: Name of VxWorks machine where to execute tests
# RemoteBinFolder: Folder of installed executables from the viewpoint of VxWorks
# RemoteTmpFolder: Some Writeable Folder from the viewpoint of VxWorks
#
# extra tests in noinst_PROGRAMS:
# testmain [-n <count>] [<filename>]
#  once per second write to stdout, because syslog (/var/log/messages) not available on VxWorks.
#  -n : iterations, default 10000. After 10 iterations reopen the log file.
#  filename: additional log, default stdout
#  (using neither syslog nor filename, results in two times logging to stdout)
# testbench [<count> [<size>]]
# Benchmark with timing.
# count: records per iteration, default=100
# size: size per record, default=128
# Always try to use CPU timestamp calibrated to 1 MSec instead of posix clock.
# Note: console I/O time is included in measurement! serial terminal vs rlogin makes a huge different!
# 
# Fixed: testbench no longer closes its own stdout which is common with the shell.
#   Therefore waiting for last line of output instead of prompt and immediate log out no longer necessary.
#	echo "expect -gl \"fprintf:\""; \
#	echo "expect -gl \"us\r\""; \
#
# Testcases of $(TESTS):
# testConfig and testPropertyConfig need additional files in current folder, see make install below
# 
# Attention! Need to unload previous instance of test before downloading again!
#
# expect has a default timeout of 10 seconds to advance to the next command without
# getting the expected input, which confuses output.
# at least testmain, ld, testConfig and testPropertyConfig (of $(TESTS) loop) are affected.
# Instead of individual timeouts 'expect -timeout 30 ...' set a global timeout 'set timeout 30'.
#
# log4cpp_testmain.log: Created at a location which might be accessible only be the remote host,
# but not by the local make file. Therefore clean up immediately after use.
# Note: With VxWorks rsh/nfs appending to a file without seek to the end might not work.
#
# Before very first acsMakefile do_all, ../bin might not exist. Create to be sure.
# Note: Failed redirection of comamnd grouping { } delivers no exit code!

.PHONY : expect	
expect :
	mkdir -p ../bin
	@echo "echo '...remote tests execution script...' >../bin/rtests.expect"
	@{ \
	echo "#!/usr/bin/expect -f"; \
	echo "set host [lindex \$$argv 0]"; \
	echo "set dir [lindex \$$argv 1]"; \
	echo "set tmp [lindex \$$argv 2]"; \
	echo "set timeout 30\r"; \
	echo "spawn rlogin \$$host"; \
	echo "expect -ex \"\\n-> \""; \
	echo "send \"cd \\\"\$$dir\\\"\r\""; \
	echo "expect -ex \"\\n-> \""; \
	echo "send \"unld \\\"log4cpptests\\\",0\r\""; \
	echo "expect -ex \"\\n-> \""; \
	echo "send \"unld \\\"log4cppcommon\\\",0\r\""; \
	echo "expect -ex \"\\n-> \""; \
	echo "send \"ld 0,0,\\\"log4cppcommon\\\"\r\""; \
	echo "expect -ex \"\\n-> \""; \
	echo "send \"ld 0,0,\\\"log4cpptests\\\"\r\""; \
	echo "expect -ex \"\\n-> \""; \
	for atest in $(TESTS); do \
		echo "send \"$$atest\r\""; \
		echo "expect -ex \"\\n-> \""; \
	done; \
	echo "send \"testmain \\\"-n 21\\\"\r\""; \
	echo "expect -timeout 30 -ex \"\\n-> \""; \
	echo "send \"rm \\\"\$$tmp/log4cpp_testmain.log\\\"\r\""; \
	echo "expect -ex \"\\n-> \""; \
	echo "send \"testmain \\\"-n 21 \$$tmp/log4cpp_testmain.log\\\"\r\""; \
	echo "expect -ex \"\\n-> \""; \
	echo "send \"copy \\\"\$$tmp/log4cpp_testmain.log\\\",0\r\""; \
	echo "expect -ex \"\\n-> \""; \
	echo "send \"rm \\\"\$$tmp/log4cpp_testmain.log\\\"\r\""; \
	echo "expect -ex \"\\n-> \""; \
	echo "send \"testbench \\\"10 5\\\"\r\""; \
	echo "expect -ex \"\\n-> \""; \
	echo "close"; \
	echo "wait"; \
	} >../bin/rtests.expect
	@[ -r ../bin/rtests.expect ] || exit 1
	chmod a+x ../bin/rtests.expect
	  
# Installing the test applications
# The configure linked applications are not useful - don't install them - this is done by calling make
# But install the extra files needed for executing the tests 
# cp -pt $(INSTALL_DIR)/vw/bin/$(CPU) $(addprefix log4cpp-$(LOG4CPP_VER)/tests/,$(addsuffix $(EXEEXT),$(TESTS)) $(addsuffix $(EXEEXT),$(noinst_PROGRAMS)) $(check_DATA))
.PHONY : install
install :
	cp -pt $(INSTALL_DIR)/vw/bin/$(CPU) $(addprefix log4cpp-$(LOG4CPP_VER)/tests/,$(check_DATA))

#___oOo___

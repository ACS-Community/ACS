#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.4 2012/11/23 08:31:03 acaproni Exp $"
#
#

###############################################
# Macro definitions.                          #
###############################################
define makeIt
   ( ( (/usr/bin/time -f "$1 COMPILATION TIME %E" make $(MAKE_FLAGS) -C $1 $2 2>&1) && ( echo "### ==> SUCCEDED" | tee -a  $4 >/dev/null ) ) || ( echo "### ==> FAILED $2 ! " | tee -a $3 $4 1>&2 ) ) | tee -a $3 $4 >/dev/null;
endef

define makeItAux
   ( ( make $(MAKE_FLAGS) -C $1 $2 2>&1 ) || ( echo "### ==> FAILED $2 ! " | tee -a $3 $4 1>&2 ) ) | tee -a $3 $4 >/dev/null;
endef
###############################################

osrev  = $(shell uname -r)
os     = $(shell uname)

#
# Modules in the various Laser sub-packages.
#
MODULES_ACSGUIS  = 	acsGUIutil \
                    acsEclipseUtils \
                    acssampGUI \
                    cdbBrowser \
                    errorBrowser \
                    eventGUI \
                    jlog \
                    logLevelGUI \
                    logTools \
                    objexp \
					alarmSourcePanel \
			AlarmSystemProfiler \
                    alarmPanel

ifeq ($(os) $(VXWORKS_RTOS),Linux NO)
    MAKE_FLAGS = "NOCCS=1"
endif
ifeq ($(os),$(CYGWIN_VER))
    MAKE_FLAGS = "NOCCS=1"
endif

SHELL=/bin/ksh
ECHO=echo

ifdef MAKE_VERBOSE
    AT = 
    OUTPUT =
else
    AT = @
    OUTPUT = > /dev/null
endif
#

startupDir = $(shell pwd)


#
# This target just forward any make target to all modules
#
define canned
	@$(ECHO) "############ Executing '$@' on all GUI modules #################"
	@for member in  $(foreach name, $(MODULES_ACSGUIS), $(name) ) ; do \
		    $(ECHO) "############ $${member}" ;\
		    if [ ! -d $${member} ]; then \
                         echo "######## ==> $${member} MODULE NOT FOUND! FAILED! " | tee -a build.log;\
                    fi;\
		    if [ -f $${member}/src/Makefile ]; then \
			$(MAKE) $(MAKE_FLAGS) -C $${member}/src/ $@ || break ;\
		    elif [ -f $${member}/ws/src/Makefile ]; then \
			$(MAKE) $(MAKE_FLAGS) -C $${member}/ws/src/ $@ || break ;\
		    fi;\
		    if [ "$(VXWORKS_RTOS)" == "YES" ]; then \
			if [ -f $${member}/lcu/src/Makefile ]; then \
			$(MAKE) $(MAKE_FLAGS) -C $${member}/lcu/src/ $@ || break ;\
			fi;\
		    fi;\
		done
endef

#
# This target builds and installs the complete Laser
# on a clean directory structure.
# Per each module it executes:
#    make clean all install
#
build: 	clean_log update
	@$(ECHO) "... done"

#
# This target builds and installs the complete Laser
# on a clean directory structure.
# Per each module it executes:
#    make clean all man install clean
#
build_clean:   	clean_log update_clean
	@$(ECHO) "... done"

#
# This target builds and installs the complete Laser 
# on a clean directory structure.
# Per each module it executes:
#    make clean all man install clean
# then, before going to the next module, trier to run the
# modular test.
# This is useful to discover circular dependencies between
# modules.
#
build_clean_test:   	clean_log update_clean_test
	@$(ECHO) "... done"

#
# This target re-builds and installs the complete Laser 
# on an existing directory structure.
# Per each module it executes:
#    make clean all man install clean
#
rebuild:	clean_log update
	@$(ECHO) "... done"

clean_log:
	@$(ECHO) "############ Clean Build Log File: build.log #################"
	@rm -f build.log
	@touch build.log

clean_test_log:
	@$(ECHO) "############ Clean Test Log File: test.log #################"
	@rm -f test.log
	@touch test.log

#
# Update of all core components
# According to SE standards does not make man and does not clenup at the end.
#
# GCH 2005-02-02
#   Added a 'true' at the end of the look to ensure
#   that is the LAST module fails the whole Make does not fail
#

update:
	@$(ECHO) "  ############ (Re-)build ACS GUIs         #################"| tee -a build.log
	@for member in  $(foreach name, $(MODULES_ACSGUIS), $(name) ) ; do \
		    if [ ! -d $${member} ]; then \
                         echo "  ######## ==> $${member} MODULE NOT FOUND! FAILED! " | tee -a build.log;\
                    fi;\
		    if [ -f $${member}/src/Makefile ]; then \
		         $(ECHO) "  ############ $${member} MAIN" | tee -a build.log;\
                         $(call makeItAux,$${member}/src,clean,build.log,$${member}/src/NORM-BUILD-OUTPUT) \
                         $(call makeIt,$${member}/src,all,build.log,$${member}/src/NORM-BUILD-OUTPUT) \
                         $(call makeItAux,$${member}/src,install,build.log,$${member}/src/NORM-BUILD-OUTPUT) \
                    elif [ -f $${member}/ws/src/Makefile ]; then \
		         $(ECHO) "  ############ $${member} WS" | tee -a build.log;\
                         $(call makeItAux,$${member}/ws/src,clean,build.log,$${member}/ws/src/NORM-BUILD-OUTPUT) \
                         $(call makeIt,$${member}/ws/src,all,build.log,$${member}/ws/src/NORM-BUILD-OUTPUT) \
                         $(call makeItAux,$${member}/ws/src,install,build.log,$${member}/ws/src/NORM-BUILD-OUTPUT) \
		    fi;\
		    if [ "$(VXWORKS_RTOS)" == "YES" ]; then \
			if [ -f $${member}/lcu/src/Makefile ]; then \
			 $(ECHO) "  ############ $${member} LCU" | tee -a build.log;\
                         $(call makeItAux,$${member}/lcu/src,clean,build.log,$${member}/lcu/src/NORM-BUILD-OUTPUT) \
                         $(call makeIt,$${member}/lcu/src,all,build.log,$${member}/lcu/src/NORM-BUILD-OUTPUT) \
                         $(call makeItAux,$${member}/lcu/src,install,build.log,$${member}/lcu/src/NORM-BUILD-OUTPUT) \
			fi;\
		    fi;\
		done;\
         true;
	@$(ECHO) "  ############ DONE (Re-)build ACS GUIs    #################"| tee -a build.log

## GCH 2003-09-03
# Changed test strategy because of problems in the interaction between make test and tat
# (see spr.2003071)
# Now if there is a WS and LCU part I do the following:
#    - make all in LCU
#    - make all in ws
#    - make test in ws
# This model assumes that tests exercising the LCU code are in any case driven by the WS side.
# This assumpion may change in the future but works now for Laser.
# We are therefore not using any more the following code:
#	$(MAKE) -k -C $${member}/ws/test/ $@ | tee -a test.log |  egrep '(Nothing to|FAILED.|PASSED.|Error:)';\
#        if [ "$(VXWORKS_RTOS)" == "YES" ]; then \
#	   if [ -d $${member}/lcu/test ]; then\
#		$(ECHO) "############ $${member}/lcu/test LCU TEST ############" | tee -a test.log;\
#		$(MAKE) -k -C $${member}/lcu/test/ $@ | tee -a test.log |  egrep '(Nothing to|FAILED.|PASSED.|Error:)';\
#	   fi;\
#        fi;\
# GCH 2004-10-21
# I have changed this again.
# Also doing a make test in ws AFTER a make all in lcu and ws does not work.
# The problem is that tat tries again to go on the LCU part and build it and
# gets confused by the environment it receives from the make command.
# The only solution seem to call tat directly.
# Now if there is a WS and LCU part I do the following:
#    - make all in LCU
#    - make all in ws
#    - tat in ws

# Update of all core components
# This does also make man and does clenup at the end.
# We use a define for the procedure of a single module, so that
# we can reuse it also in the update_clean_test target further down.

define update-clean-one-module
		    if [ ! -d $${member} ]; then \
                         echo "  ######## ==> $${member} MODULE NOT FOUND! FAILED! " | tee -a build.log;\
                    fi;\
		    if [ -f $${member}/src/Makefile ]; then \
		         $(ECHO) "  ############ $${member} MAIN" | tee -a build.log;\
                         $(MAKE) $(MAKE_FLAGS) -C $${member}/src/ clean >> build.log 2>& 1;\
                         $(MAKE) $(MAKE_FLAGS) -C $${member}/src/ all >> build.log 2>& 1 || echo "### ==> FAILED all ! " | tee -a build.log; \
                         $(MAKE) $(MAKE_FLAGS) -C $${member}/src/ man >> build.log 2>& 1 || echo "### ==> FAILED man ! " | tee -a build.log; \
                         $(MAKE) $(MAKE_FLAGS) -C $${member}/src/ install >> build.log 2>& 1 || echo "### ==> FAILED install ! " | tee -a build.log; \
                         $(MAKE) $(MAKE_FLAGS) -C $${member}/src/ clean >> build.log 2>& 1 || echo "### ==> FAILED clean ! " | tee -a build.log; \
		    elif [ -f $${member}/ws/src/Makefile ]; then \
		         $(ECHO) "  ############ $${member} WS" | tee -a build.log;\
                         $(MAKE) $(MAKE_FLAGS) -C $${member}/ws/src/ clean >> build.log 2>& 1;\
                         $(MAKE) $(MAKE_FLAGS) -C $${member}/ws/src/ all >> build.log 2>& 1 || echo "### ==> FAILED all ! " | tee -a build.log; \
                         $(MAKE) $(MAKE_FLAGS) -C $${member}/ws/src/ man >> build.log 2>& 1 || echo "### ==> FAILED man ! " | tee -a build.log; \
                         $(MAKE) $(MAKE_FLAGS) -C $${member}/ws/src/ install >> build.log 2>& 1 || echo "### ==> FAILED install ! " | tee -a build.log; \
                         $(MAKE) $(MAKE_FLAGS) -C $${member}/ws/src/ clean >> build.log 2>& 1 || echo "### ==> FAILED clean ! " | tee -a build.log; \
		    fi;\
		    if [ "$(VXWORKS_RTOS)" == "YES" ]; then \
			if [ -f $${member}/lcu/src/Makefile ]; then \
			 $(ECHO) "  ############ $${member} LCU" | tee -a build.log;\
                         $(MAKE) $(MAKE_FLAGS) -C $${member}/lcu/src/ clean >> build.log 2>& 1;\
                         $(MAKE) $(MAKE_FLAGS) -C $${member}/lcu/src/ all >> build.log 2>& 1 || echo "### ==> FAILED all ! " | tee -a build.log; \
                         $(MAKE) $(MAKE_FLAGS) -C $${member}/lcu/src/ man >> build.log 2>& 1 || echo "### ==> FAILED man ! " | tee -a build.log; \
                         $(MAKE) $(MAKE_FLAGS) -C $${member}/lcu/src/ install >> build.log 2>& 1 || echo "### ==> FAILED install ! " | tee -a build.log; \
                         $(MAKE) $(MAKE_FLAGS) -C $${member}/lcu/src/ clean >> build.log 2>& 1 || echo "### ==> FAILED clean ! " | tee -a build.log; \
			fi;\
		    fi
endef

#
#
# GCH 2005-02-02
#   Added a 'true' at the end of the look to ensure
#   that is the LAST module fails the whole Make does not fail
#
update_clean:
	@$(ECHO) "  ############ (Re-)build ACS GUIs          #################"| tee -a build.log
	@for member in  $(foreach name, $(MODULES_ACSGUIS), $(name) ) ; do \
                   $(update-clean-one-module);\
		done;\
         true;
	@$(ECHO) "  ############ DONE (Re-)build ACS GUIs     #################"| tee -a build.log

#
# Test target
#

#
# Check if the ACS_INSTANCE variable is defined
# This allows to check for a clean test environment.
#
ifdef ACS_INSTANCE
    MAKE_TEST_ACS_INSTANCE = $(ACS_INSTANCE)
else
    MAKE_TEST_ACS_INSTANCE = 0
endif

# We use a define for the procedure of a single module, so that
# we can reuse it also in the update_clean_test target further down.

define test-one-module
		if [ -d $(ACSDATA)/tmp/ACS_INSTANCE.$(MAKE_TEST_ACS_INSTANCE) ]; then \
			$(ECHO) "############ $${member}: WARNING: ACS_INSTANCE.$(MAKE_TEST_ACS_INSTANCE) NOT CLEAN!  ############" | tee -a test.log ;\
		fi;\
		if [ -f $${member}/ws/test/Makefile ]; then\
			$(ECHO) "############ $${member}/ws/test WS TEST ############" | tee -a test.log ;\
                        if [ -f $${member}/lcu/test/Makefile ]; then \
                           $(MAKE) $(MAKE_FLAGS) -C $${member}/lcu/test/ all >> test.log 2>& 1 || echo "### ==> FAILED all lcu/test/! " | tee -a test.log; \
                        fi;\
                        $(MAKE) $(MAKE_FLAGS) -C $${member}/ws/test/ all >> test.log 2>& 1 || echo "### ==> FAILED all ws/test/! " | tee -a test.log; \
                        cd $${member}/ws/test/; export MAKE_PURE; tat -nc -v 2>& 1 | tee -a test.log |  egrep  '(Nothing to|FAILED.|PASSED.|Error:)'; cd $(startupDir); \
		elif [ -f $${member}/test/Makefile ]; then\
			$(ECHO) "############ $${member}/test MAIN TEST ############" | tee -a test.log ;\
			$(MAKE) -k -C $${member}/test/ test | tee -a test.log |  egrep  '(Nothing to|FAILED.|PASSED.|Error:)';\
		else\
			$(ECHO) "######## ==> $${member} TEST DIRECTORY STRUCTURE NOT FOUND! CANNOT TEST ANYTHING!" | tee -a test.log ;\
		fi
endef

.PHONY: test

#
# If running on a machine where VxWorks is ocnfigured and tests have to be urn on the LCU.
# $(ACCDB_CONFIG) will be defined and the target accdb_config will be called to
# reconfigure the acc database in a clean way.
#
Test = test
$(Test): clean_test_log $(ACCDB_CONFIG)
	@$(ECHO) "############ TEST ACS GUIs #################"| tee -a test.log
	@for member in $(foreach name,$(MODULES_ACSGUIS),$(name)); do\
                   $(test-one-module);\
	done;\
        true;
	@$(ECHO) "############ DONE TEST ACSGUIs    #################"| tee -a test.log


update_clean_test: clean_test_log $(ACCDB_CONFIG)
	@$(ECHO) "############ UPDATE and TEST ACS GUIs #################"| tee -a build.log
	@for member in $(foreach name,$(MODULES_ACSGUIS),$(name)); do\
                   $(update-clean-one-module);\
                   $(test-one-module);\
	done;\
        true;
	@$(SHELL)  $(MODULE_PREFIX)/acsBUILD/src/acsBUILDAfterBuildMod.sh >> build.log 2>& 1
	@$(ECHO) "############ DONE UPDATE and TEST ACS GUIs    #################"| tee -a build.log


#
# show_modules target
#
# Simply lists all MODULES_ACSGUIS that would be build
# with the current setup
#
show_modules:
	@$(ECHO) "Modules in build list are:" 
	@$(ECHO) ${MODULES_ACSGUIS}

#
# Standard targets
#
clean:	
	$(canned)
all:	
	$(canned)
install:	
	$(canned)

man:
	$(canned)

#
# ___oOo___

#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) European Southern Observatory, 2007 
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
# "@(#) $Id: Makefile,v 1.4 2012/11/23 08:36:12 acaproni Exp $"
#
# Makefile of containerTests (the package dealing with container tests)
#
# who       when        what
# --------  ----------  ----------------------------------------------
# eallaert  2007-11-13  created
# eallaert  2007-12-14  updated to work with ACS/Makefile
#
#*******************************************************************************
# This Makefile follows ACS Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

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

#
# Modules in the various containerTest sub-packages.
#
MODULES_CTEST = monicd moncollect monblobber moncontroller
SUBSYSTEM = "monitoring"

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
os     = $(shell uname)
osrev  = $(shell uname -r)

startupDir = $(shell pwd)

#
# This target just forward any make target to all modules
#
define canned
	@$(ECHO) "############ Executing '$@' on all $(SUBSYSTEM) modules #################"
	@for member in  $(foreach name, $(MODULES_CTEST), $(name) ) ; do \
		    $(ECHO) "############ $${member}" ;\
		    if [ ! -d $${member} ]; then \
                         echo "### ==> $${member} MODULE NOT FOUND! FAILED! " | tee -a build.log;\
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
# This target builds and installs all containerTest modules on a clean
# directory structure.
# Per each module it executes:
#    make clean all install
#
build: 	clean_log update
	@$(ECHO) "... done"

#
# This target builds and installs all containerTest modules on a clean
# directory structure.
# Per each module it executes:
#    make clean all man install clean
#
build_clean:   	clean_log update_clean
	@$(ECHO) "... done"

#
# This target builds and installs all containerTest modules on a clean 
# directory structure.
# Per each module it executes:
#    make clean all man install clean
# then, before going to the next module, tries to run the
# modular test.
# This is useful to discover circular dependencies between
# modules.
#
build_clean_test:   	clean_log update_clean_test
	@$(ECHO) "... done"

#
# This target re-builds and installs all containerTest modules on an  
# existing directory structure.
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
# There is a 'true' at the end of the loop to ensure  that if the LAST
#  module fails the whole Make does not fail
#

update:
	@$(ECHO) "  ############ (Re-)build $(SUBSYSTEM) Software         #################"| tee -a build.log
	@for member in  $(foreach name, $(MODULES_CTEST), $(name) ) ; do \
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
	@$(ECHO) "  ############ DONE (Re-)build $(SUBSYSTEM) Software    #################"| tee -a build.log
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

# There is a 'true' at the end of the loop to ensure that if the LAST
# module fails the whole Make does not fail
#
update_clean:
	@$(ECHO) "  ############ (Re-)build $(SUBSYSTEM) Software         #################"| tee -a build.log
	@for member in  $(foreach name, $(MODULES_CTEST), $(name) ) ; do \
            $(update-clean-one-module);\
	done;\
        true;
	@$(ECHO) "  ############ DONE (Re-)build $(SUBSYSTEM) Software    #################"| tee -a build.log
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
	    $(ECHO) "\t############ $${member}/ws/test WS TEST ############" | tee -a test.log ;\
            if [ -f $${member}/lcu/test/Makefile ]; then \
                $(MAKE) $(MAKE_FLAGS) -C $${member}/lcu/test/ all >> test.log 2>& 1 || echo "### ==> FAILED all lcu/test/! " | tee -a test.log; \
            fi;\
            $(MAKE) $(MAKE_FLAGS) -C $${member}/ws/test/ all >> test.log 2>& 1 || echo "### ==> FAILED all ws/test/! " | tee -a test.log; \
            cd $${member}/ws/test/; export MAKE_PURE; tat -nc -v 2>& 1 | tee -a test.log |  egrep  '(Nothing to|FAILED.|PASSED.|Error:)'; cd $(startupDir); \
	elif [ -f $${member}/test/Makefile ]; then\
	    $(ECHO) "\t############ $${member}/test MAIN TEST ############" | tee -a test.log ;\
	    $(MAKE) -k -C $${member}/test/ test | tee -a test.log |  egrep  '(Nothing to|FAILED.|PASSED.|Error:)';\
	else\
	    $(ECHO) "\t######## ==> $${member} TEST DIRECTORY STRUCTURE NOT FOUND! CANNOT TEST ANYTHING!" | tee -a test.log ;\
	fi
endef

.PHONY: test

Test = test
#
# If running on a machine where VxWorks is ocnfigured and tests have to be urn on the LCU.
# $(ACCDB_CONFIG) will be defined and the target accdb_config will be called to
# reconfigure the acc database in a clean way.
#
$(Test): clean_test_log $(ACCDB_CONFIG)
	@$(ECHO) "############ TEST $(SUBSYSTEM) Software #################"| tee -a test.log
	@for member in $(foreach name,$(MODULES_CTEST),$(name)); do\
                   $(test-one-module);\
	done;\
        true;
	@$(ECHO) "############ DONE TEST $(SUBSYSTEM) Software    #################"| tee -a test.log


update_clean_test: clean_test_log $(ACCDB_CONFIG)
	@$(ECHO) "############ UPDATE and TEST $(SUBSYSTEM) Software #################"| tee -a build.log
	@for member in $(foreach name,$(MODULES_CTEST),$(name)); do\
                   $(update-clean-one-module);\
                   $(test-one-module);\
	done;\
        true;
	@$(SHELL)  $(MODULE_PREFIX)/acsBUILD/src/acsBUILDAfterBuildMod.sh >> build.log 2>& 1
	@$(ECHO) "############ DONE UPDATE and TEST $(SUBSYSTEM) Software    #################"| tee -a build.log


#
# show_modules target
#
# Simply lists all MODULES_CTEST that would be build
# with the current setup
#
show_modules:
	@$(ECHO) "Modules in build list are:" 
	@$(ECHO) ${MODULES_CTEST}

#
# Standard canned targets
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

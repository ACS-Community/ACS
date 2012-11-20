#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# Copyright (c) European Southern Observatory, 2012 
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
# "@(#) $Id: Makefile,v 1.35 2012/11/20 17:31:44 hsommer Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# almamgr  2012-11-16  created
#
#*******************************************************************************
# This Makefile follows ACS Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

#
# Modules in the various sub-packages.
#
# The following example is for a subsystem called Voyager One
# which is composed of two directories, Alpha and Beta.

# Alpha contains the modules alpha1, alpha2 and alpha3
# Beta continas the moduels beta1, beta2 and beta3
#
# Modify as appropriate to your subsystem

SUBSYSTEM = "ACSLaser"

MODULES_ACSLASER  = 	alarmCommon \
			laser-extlib \
			acs-jms \
			cmw-mom \
			laser-util \
			laser-source \
			laser-source-cpp \
			laser-source-python \
			gp-openide \
			gp \
			laser-core \
			alarmHibernate \
			laser-client \
			laser-definition \
			laser-console \
			alarm-clients \
			demo \
			managerTest \
			baciPropsTest \
			alarmTests \
			containerTest

MODULES =  $(foreach dir, $(MODULES_ACSLASER), $(dir)) 

#
# If option KEEP_GOING=on is present in the make command line gnu_make is NOT interrupted 
# when the first error is encountered
#

ifdef KEEP_GOING
   KEEP_GOING="on"
else
   KEEP_GOING="off"
endif

RETURN_CODE=return_code
TMP_RETURN_CODE=tmp_return_code

MAKE_FLAGS = "-k"
PLATFORM := $(shell uname)

SHELL=/bin/bash
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

ifeq ($(os),SunOS)
	realtime=YES
endif

#
# "Failed all" error management
#
define mng_failed_all
	if [[ -a  $(TMP_RETURN_CODE) ]]; then\
		$(ECHO) "### ==> FAILED all ! " | tee -a build.log | tee -a $(RETURN_CODE);\
		rm $(TMP_RETURN_CODE);\
		if [[ $(KEEP_GOING) = "off" ]]; then \
			if [[ -a $(RETURN_CODE) ]]; then \
				rm $(RETURN_CODE);\
			fi;\
			exit 2;\
		fi;\
	fi 
endef

#
# "Failed install" error management
#
define mng_failed_install
	if [[ -a  $(TMP_RETURN_CODE) ]]; then\
		$(ECHO) "### ==> FAILED install ! " | tee -a build.log | tee -a $(RETURN_CODE);\
		rm $(TMP_RETURN_CODE);\
		if [[ $(KEEP_GOING) = "off" ]]; then \
			if [[ -a $(RETURN_CODE) ]]; then \
				rm $(RETURN_CODE);\
			fi;\
			exit 2;\
		fi;\
	fi 
endef


#
# This target just forward any make target to all modules
#
define canned
	@$(ECHO) "############ Executing '$@' on all $(SUBSYSTEM) modules #################"
	@for member in  $(foreach name, $(MODULES), $(name) ) ; do \
		    $(ECHO) "############ $${member}" ;\
		    if [ ! -d $${member} ]; then \
                         echo "### ==> $${member} MODULE NOT FOUND! FAILED! " | tee -a build.log;\
                    fi;\
		    if [ -f $${member}/src/Makefile ]; then \
			$(MAKE) $(MAKE_FLAGS) -C $${member}/src/ $@ || break ;\
		    elif [ -f $${member}/ws/src/Makefile ]; then \
			$(MAKE) $(MAKE_FLAGS) -C $${member}/ws/src/ $@ || break ;\
		    fi;\
		    if [ "$(realtime)" == "YES" ]; then \
			if [ -f $${member}/lcu/src/Makefile ]; then \
			$(MAKE) $(MAKE_FLAGS) -C $${member}/lcu/src/ $@ || break ;\
			fi;\
		    fi;\
		done
endef

clean_log:
	@$(ECHO) "############ Clean Build Log File: build.log #################"
	@rm -f build.log
	@touch build.log

update: build

#
# building all modules
#
build:	
	@$(ECHO) "############ build $(SUBSYSTEM) Software         #################"| tee -a build.log
	@# Deletion of temporary files used to store make return code
	@if [[ -a $(TMP_RETURN_CODE) ]]; then \
		rm $(TMP_RETURN_CODE);\
	fi
	@if [[ -a $(RETURN_CODE) ]]; then \
		rm $(RETURN_CODE);\
	fi
	@for member in  $(foreach name, $(MODULES), $(name) ) ; do \
		    if [ ! -d $${member} ]; then \
			echo "### ==> $${member} MODULE NOT FOUND! FAILED! " | tee -a build.log;\
		    fi;\
		    if [ -f $${member}/src/Makefile ]; then \
		         $(ECHO) "############ $${member} MAIN" | tee -a build.log;\
			 $(MAKE) $(MAKE_FLAGS) -C $${member}/src/ clean >> build.log 2>& 1;\
			 $(MAKE) $(MAKE_FLAGS) -C $${member}/src/ all >> build.log 2>& 1 || echo $$? >> $(TMP_RETURN_CODE) ;\
			 $(mng_failed_all);\
			 $(MAKE) $(MAKE_FLAGS) -C $${member}/src/ install >> build.log 2>& 1 || echo $$? >> $(TMP_RETURN_CODE) ;\
			 $(mng_failed_install);\
			 continue ;\
		    fi;\
		    if [ -f $${member}/ws/src/Makefile ]; then \
			 $(ECHO) "############ $${member} WS" | tee -a build.log;\
			 $(MAKE) $(MAKE_FLAGS) -C $${member}/ws/src/ clean >> build.log 2>& 1;\
			 $(MAKE) $(MAKE_FLAGS) -C $${member}/ws/src/ all >> build.log 2>& 1 || echo $$? >> $(TMP_RETURN_CODE) ;\
			 $(mng_failed_all);\
			 $(MAKE) $(MAKE_FLAGS) -C $${member}/ws/src/ install >> build.log 2>& 1 || echo $$? >> $(TMP_RETURN_CODE) ;\
			 $(mng_failed_install);\
		    fi;\
		    if [ "$(realtime)" == "YES" ]; then \
			if [ -f $${member}/lcu/src/Makefile ]; then \
			    $(ECHO) "############ $${member} LCU" | tee -a build.log;\
			    $(MAKE) $(MAKE_FLAGS) -C $${member}/lcu/src/ clean >> build.log 2>& 1;\
			    $(MAKE) $(MAKE_FLAGS) -C $${member}/lcu/src/ all >> build.log 2>& 1 || echo $$? >> $(TMP_RETURN_CODE) ;\
			    $(mng_failed_all);\
			    $(MAKE) $(MAKE_FLAGS) -C $${member}/lcu/src/ install >> build.log 2>& 1 || echo $$? >> $(TMP_RETURN_CODE) ;\
			    $(mng_failed_install);\
			fi;\
		    fi;\
		done
		@$(ECHO) "... done"
#
# Test target
#

.PHONY: test

Test = test
$(Test):
	@$(ECHO) "############ Clean Test Log File: test.log #################"
	@rm -f test.log
	@touch test.log
	@$(ECHO) "############ TEST $(SUBSYSTEM) Software #################"| tee -a test.log
	@for member in $(foreach name,$(MODULES),$(name)); do\
		if [ -d $${member}/ws/test ]; then\
			$(ECHO) "############ $${member}/ws/test WS TEST ############" | tee -a test.log ;\
			$(MAKE) -k -C $${member}/ws/test/ $@ | tee -a test.log | egrep '(Nothing to|FAILED.|PASSED.|Error:)';\
			if [ -d $${member}/lcu/test ]; then\
				$(ECHO) "############ $${member}/lcu/test LCU TEST ############" | tee -a test.log;\
				$(MAKE) -k -C $${member}/lcu/test/ $@ | tee -a test.log | egrep '(Nothing to|FAILED.|PASSED.|Error:)';\
			fi;\
		elif [ -d $${member}/test ]; then\
			$(ECHO) "############ $${member}/test MAIN TEST ############" | tee -a test.log ;\
			$(MAKE) -k -C $${member}/test/ $@ | tee -a test.log | egrep '(Nothing to|FAILED.|PASSED.|Error:)';\
		else\
			$(ECHO) "### ==> $${member} TEST DIRECTORY STRUCTURE NOT FOUND! FAILED!" | tee -a test.log ;\
		fi;\
	done
	@$(ECHO) "... done"
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

buildClean: build clean

buildMan: build man

#
# ___oOo___

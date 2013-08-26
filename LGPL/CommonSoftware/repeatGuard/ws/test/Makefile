#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2007 
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
# "@(#) $Id: Makefile,v 1.6 2012/01/24 01:00:05 tstaig Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# nbarriga  2007-02-20  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

#
# user definable C-compilation flags
USER_CFLAGS = -Wall

#
# Type safe log definitions
# --------------------------
ACSLOGTSDEF=repeatGuardLogTypeExample

#
# Jarfiles and their directories
#
JARFILES=repeatGuardJavaTest
repeatGuardJavaTest_DIRS=alma
jjj_EXTRAS= 

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     = testRepeatGuard testRepeatGuardLogger
EXECUTABLES_L   = 

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         =
SCRIPTS_L    	= repeatGuardJavaTest #repeatGuardJavaLoggerTest
#
# <brief description of xxxxx program>
testRepeatGuard_OBJECTS   =	testRepeatGuard
testRepeatGuard_LIBS      = RepeatGuard acsutil logging

testRepeatGuardLogger_OBJECTS   = testRepeatGuardLogger
testRepeatGuardLogger_LIBS   = RepeatGuardLogger RepeatGuard repeatGuardLogTypeExampleLTS maciClient

#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach rtos, $(RTAI_MODULES) , $($(rtos)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

#
#>>>>> END OF standard rules

#
# INCLUDE STANDARDS
# -----------------

MAKEDIRTMP := $(shell searchFile include/acsMakefile)
ifneq ($(MAKEDIRTMP),\#error\#)
   MAKEDIR := $(MAKEDIRTMP)/include
   include $(MAKEDIR)/acsMakefile
endif

#
# TARGETS
# -------
all:	do_all
	@mkdir -p tmp
	@echo " . . . 'all' done" 

clean : clean_all 
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"


#___oOo___

#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) European Southern Observatory, 2009 
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
# "@(#) $Id: Makefile,v 1.3 2012/03/26 12:56:11 rtobar Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# almadev  2009-10-11  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------


#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = eventGUI
SCRIPTS_L       =

#
# Jarfiles and their directories
#
JARFILES= 
jjj_DIRS=
jjj_EXTRAS= 
#
# java sources in Jarfile on/off
DEBUG= 

#
# other files to be installed
#----------------------------
INSTALL_FILES =

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
#

# Determine the host architecture and use it to decide which version of the
# product should we install
ARCH=$(shell getconf LONG_BIT)

# We only support 32 and 64 bits builds
ifneq ($(ARCH),32)
ifneq ($(ARCH),64)
	$(error Unsupported architecture: "$(ARCH)")
endif
endif

# Deduce the zipfile suffix
ifeq ($(ARCH), 32)
   ZIP_SUFFIX=x86
else
   ZIP_SUFFIX=x86_64
endif

all:	do_all
	ant init createJarsPlugin pde-build
	@echo " . . . 'all' done" 

clean : clean_all 
	ant clean
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@INSTALL_DIR=$$(searchFile bin/eventGUI); \
	 echo "Will install to $$INSTALL_DIR/lib/ACS_eventGUI"; \
	 cd $$INSTALL_DIR/lib; \
	 rm -rf ACS_eventGUI; \
	 unzip -o $(PWD)/../object/headlessTemp/I.eventGUIBuild/eventGUIBuild-linux.gtk.$(ZIP_SUFFIX).zip;
	@echo " . . . installation done"


#___oOo___

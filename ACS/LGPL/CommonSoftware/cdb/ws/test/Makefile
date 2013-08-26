#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) European Southern Observatory, 2002
# Copyright by ESO (in the framework of the ALMA collaboration)
# and Cosylab 2002, All rights reserved
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307  USA
#
# "@(#) $Id: Makefile,v 1.48 2012/01/21 22:48:11 tstaig Exp $" 
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi 2003-03-11 Create ref directory if does not exist (because taken from CVS)
# gchiozzi 2003-03-11 Removed baci library.
# gchiozzi 2002-12-05 Commented out CCS libraries and CCS database files.
# gchiozzi 2002-09-30 SCRIPTS_L: added cdbTestJClient, removed start
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

USER_CFLAGS =
USER_LIB =

# Program to build
# Can be overwritten in the call

PROG = 

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES_L     = $(PROG) cdbTest nodeTest  



#
# <brief description of xxxxx program>


$(PROG)_OBJECTS            = $(PROG)
$(PROG)_LDFLAGS            =
$(PROG)_LIBS               = C++

nodeTest_OBJECTS		   = nodeTest
nodeTest_LIBS           = cdb
cdbTest_OBJECTS            = cdbTest
cdbTest_LIBS         = cdb


#
# Includes (.h) files (public and local)
# ---------------------------------
INCLUDES        =
INCLUDES_L      =

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       = 
LIBRARIES_L     =

#
# Jarfiles and their directories
#
JARFILES = JDALTests PerformanceTests
JDALTests_DIRS = com
PerformanceTests_DIRS = alma

#
# Scripts (public and local)
# ----------------------------
SCRIPTS_L         = cdbTestJClient cdbTATHelper cdbTestSetDefaultComponent cdbTestClearCache cdbTATRead cdbTATWrite

#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     =
TCL_SCRIPTS_L   =

#
# TCL libraries (public and local)
# ------------------------------
TCL_LIBRARIES   =
TCL_LIBRARIES_L =

#
# <brief description of tttlll library>
tttlll_OBJECTS  = 


#
# man pages to be done
# --------------------
MANSECTIONS =
MAN1 =
MAN3 =
MAN5 =
MAN7 =
MAN8 =

#
# local man pages
# ---------------
MANl =

#
# ASCII file to be converted into Framemaker-MIF
# --------------------
ASCII_TO_MIF = 


#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

#
# IDL FILES
#
IDL_FILES =

#
#>>>>> END OF standard rules

#
# CHOSE PLATFORM
# --------------
# Default is UNIX, for VxVorks application next line MUST BE UNCOMMENTED
#MAKE_VXWORKS = on   

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
	if [ $(WIND_BASE) ]; then \
	  cp -f TestList_BOTH.lite TestList.lite; \
          mkdir -p ref; \
          cp -f ref_BOTH/*.ref ref ;\
	  mkdir -p tmp ;\
	else\
	   cp -f TestList_WS.lite TestList.lite; \
           mkdir -p ref; \
	   cp -f ref_WS/*.ref ref ;\
           mkdir -p tmp ;\
	fi;
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) iors.dat *~ ../include/*~ ../idl/*~ ../*~ DAL.ior core
	$(RM) TestList.lite ref tatlogs tmp
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#___oOo___

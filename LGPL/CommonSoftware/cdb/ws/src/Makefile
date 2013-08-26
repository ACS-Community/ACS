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
# "@(#) $Id: Makefile,v 1.50 2012/01/20 23:18:16 tstaig Exp $" 
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# cparedes 2006-02-28 Added cdbSetDefaultComponent
# almamgr 2003-09-01 Cleaned up USER-INC
# gchiozzi 2003-07-07 Added cdbjDALClearCache script
# gchiozzi 2003-01-28 Creates lib/endorsed if does not exist
# gchiozzi 2002-12-05 Added cdbRead script
# gchiozzi 2002-10-16 Modified for new CDB to install schema files.
# gchiozzi 2002-10-02 Added handling of xercesImpl.jar and xmlParserAPIs.jar as endorsed libraries.
# gchiozzi 2002-09-30 Added cdbjDAL script
# gchiozzi 2002-09-30 Added Java DAL code
# gchiozzi 2002-07-12 Added clean_dist target.
# gchiozzi 2002-07-12 Added cdbDAOImpl.h in INCLUDES list
# almamgr 2000-07-20 created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

USER_CFLAGS =
USER_LIB =
#USER_INC =

# Program to build
# Can be overwritten in the call

PROG = 



#
# additional include and library search paths
#USER_LIB = 

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     = $(PROG)

EXECUTABLES_L   = 

#
# <brief description of xxxxx program>

$(PROG)_OBJECTS            = $(PROG)
$(PROG)_LDFLAGS            =
$(PROG)_LIBS               = $(TESTLIBS) C++

#
# special compilation flags for single c sources
#yyyyy_CFLAGS   = 

#
# Includes (.h) files (public and local)
# ---------------------------------
INCLUDES        = cdbField.h cdb.h cdb.i cdbData_Types.h cdbINIReader.h cdbExport.h cdbDALaccess.h cdbDAOImpl.h cdbDAONode.h cdbDAOProxy.h
# GCH: CCS database not used any more for the time being
# INCLUDES        = cdbCCS.h cdb.i cdbField.h cdb.h cdbData_Types.h cdbINIReader.h cdbExport.h cdbDALaccess.h cdbDAOImpl.h
# GCH: CCS database not used any more for the time being
#INCLUDES_L      = dbPrivate.h
INCLUDES_L      = 
#
# Libraries (public and local)
# ----------------------------
LIBRARIES       = cdb
LIBRARIES_L     =

# GCH: CCS database not used any more for the time being
# cdb_OBJECTS   = cdbCCS cdbField cdb cdbIMDB cdbINIReader cdbDALaccess cdbDAOImpl
cdb_OBJECTS   = cdbField cdb cdbIMDB cdbINIReader cdbDALaccess cdbDAOImpl cdbDAONode cdbDAOProxy
cdb_LIBS      = cdbDALStubs ACSErrTypeCommon ACSErrTypeCORBA cdbErrType acscomponentStubs logging expat
cdb_CFLAGS    = -DCDB_BUILD_DLL

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = cdbjDAL cdbjDALShutdown cdbjDALClearCache cdbRead cdbWrite cdbTATPrologue cdbTATEpilogue cdbSetDefaultComponent cdbChangeComponentDeployment cdbAddImplLang.py 

SCRIPTS_L       =

#
# Java code
# ------------------------------

#
# This will have to be replaced by:
# JAVA_ENDORSED=1
# when implemented
#
XERCES_JAR = $(shell if [ -f $(INTROOT)/lib/endorsed/xercesImpl.jar ]; then \
echo $(INTROOT)/lib/endorsed/xercesImpl.jar; \
else \
echo $(ACSROOT)/lib/endorsed/xercesImpl.jar; \
fi;)
XMLPARSER_JAR = $(shell if [ -f $(INTROOT)/lib/endorsed/xmlParserAPIs.jar ]; then \
echo $(INTROOT)/lib/endorsed/xmlParserAPIs.jar; \
else \
echo $(ACSROOT)/lib/endorsed/xmlParserAPIs.jar; \
fi;)
CLASSPATH := $(CLASSPATH):$(XERCES_JAR)



DEBUG=on

JARFILES = CDB
CDB_DIRS = com

#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     =
TCL_SCRIPTS_L   =

#
# <brief description of tttttt tcl-script>
tttttt_OBJECTS  =
tttttt_TCLSH    = 
tttttt_LIBS     = 

#
# TCL libraries (public and local)
# ------------------------------
TCL_LIBRARIES   =
TCL_LIBRARIES_L =

#
# <brief description of tttlll library>
tttlll_OBJECTS  = 

#
# Configuration Database Schema Files
# ----------------------
CDB_SCHEMAS =

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
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) *~ ../include/*~ ../dbl/*~ ../idl/*~ ../*~ ../../*~ core ../*/DAL.ior
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#___oOo___

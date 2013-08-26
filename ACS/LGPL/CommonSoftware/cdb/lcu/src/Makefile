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
# "@(#) $Id: Makefile,v 1.35 2006/10/13 06:39:03 bjeram Exp $" 
#
# Makefile of cdb
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi 2002-12-05 Commented out CCS libraries and CCS database files.
# bjeram 2002-08-07 support for DAL
# bgustafs 2001-07-12 updated to cdb 1.10, no debug
# bgustafs 2001-06-26 updated to cdb 1.8
# almamgr 2000-07-20 created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

OPTIMIZE=0

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     = cdb $(PROG)

EXECUTABLES_L   = 

#
# <brief description of xxxxx program>
# GCH: CCS database not used any more for the time being
#cdb_OBJECTS_COMMON = cdbCCS cdbField cdb cdbIMDB cdbINIReader cdbDALaccess cdbDAOImpl cdbDAONode cdbDAOProxy
cdb_OBJECTS_COMMON = cdbField cdb cdbIMDB cdbINIReader cdbDALaccess cdbDAOImpl cdbDAONode cdbDAOProxy
cdb_OBJECTS   =  $(cdb_OBJECTS_COMMON)

$(PROG)_OBJECTS            = $(PROG)
$(PROG)_LDFLAGS            =
$(PROG)_LIBS               = $(TESTLIBS) C++

#
# special compilation flags for single c sources
#yyyyy_CFLAGS   = 

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
# handle links to common source files (in MODNAME/ws/src)
#
LINK_FILES= $(addprefix ../idl/, $(addsuffix .idl, $(IDL_FILES))) \
	    $(addsuffix .cpp, $(cdb_OBJECTS_COMMON)) \
	    $(addprefix ../idl/, $(addsuffix .xml, $(ACSERRDEF)))
#
#>>>>> END OF standard rules

#
# CHOSE PLATFORM
# --------------
# Default is UNIX, for VxVorks application next line MUST BE UNCOMMENTED
MAKE_VXWORKS = on   

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
all:	do_links do_all
	@echo " . . . 'all' done" 

clean : clean_links clean_all 
	$(RM) *~ ../include/*~ ../dbl/*~ ../idl/*~ ../*~ core
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#___oOo___

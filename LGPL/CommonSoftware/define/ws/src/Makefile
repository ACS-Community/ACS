#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) European Southern Observatory, 2002
# Copyright by ESO (in the framework of the ALMA collaboration),
# All rights reserved
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
# "@(#) $Id: Makefile,v 1.16 2003/03/21 10:23:23 rgeorgie Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# david 2003-06-17 created client C++ shared libraries 
# hsommer  20/01/03  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

DEBUG = on
COMPONENT_HELPERS=on


# 
# XML schema files and includes
# 
XSDBIND = systementities

XML_IDL = "IdentifierRange=alma.archive.range.IdentifierRange"


# error definitions to generate ACS-style exceptions from
ACSERRDEF = ArchiveIdentifierError

# 
# IDL Files and flags
# 
IDL_FILES = xmlentity archive_xmlstore_if 
IDL_TAO_FLAGS =
USER_IDL =
archive_xmlstore_ifStubs_LIBS = acscommonStubs acscomponentStubs xmlentityStubs

# Libraries (public and local)
# ----------------------------
LIBRARIES       = xmlentity archive_xmlstore_if
LIBRARIES_L     =

# the following is not necessary because the acs make file creates xmlentityStubs library automatically
# but we leave it here for now to avoid troubles
xmlentity_OBJECTS           =
xmlentity_LIBS := xmlentityStubs

archive_xmlstore_if_OBJECTS = 
archive_xmlstore_if_LIBS := archive_xmlstore_ifStubs

# adds the IdentifierRange schema to archive_xmlstore_if.jar to ensure that it can be loaded again when the archive has been erased.
archive_xmlstore_if_EXTRAS = -C ../idl/IdentifierRange.xsd

#
# Jarfiles and their directories
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
	@jar -uf ../lib/archive_xmlstore_if.jar -C ../idl IdentifierRange.xsd
	@echo " . . . 'all' done" 

clean : clean_all 
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

db : db_all
	@echo " . . . ../DB done"

#___oOo___

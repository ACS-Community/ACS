#*************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2002  
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
#*******************************************************************************
# "@(#) $Id: Makefile,v 1.4 2011/03/10 15:15:53 jagonzal Exp $"
#------------------------------------------------------------------------
#
# Makefile for acsjms/src

# user definable C-compilation flags
USER_CFLAGS =
USER_LIB    =

# C programs (public and local)
# -----------------------------
EXECUTABLES     =
EXECUTABLES_L   =

# Includes (.h) files (public only)
# ---------------------------------
INCLUDES   =
INCLUDES_L =

# Libraries (public and local)
# ----------------------------
LIBRARIES       =
LIBRARIES_L     =

acsjms_OBJECTS  =
acsjms_LIBS     =

# Jarfiles
JARFILES      = acsjms
acsjms_DIRS   = com
acsjms_EXTRAS =
acsjms_JLIBS  = ACSJMSMessageEntity

# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

# IDL Files
IDL_FILES =  ACSJMSMessageEntity
USER_IDL  =

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
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) *~ ../include/*~ ../dbl/*~ ../idl/*~ ../*~ ../../*~ core*
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

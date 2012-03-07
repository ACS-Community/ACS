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
# "@(#) $Id: Makefile,v 1.33 2012/03/07 17:43:16 acaproni Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi  11/02/02  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

PY_SCRIPTS  = ACSPyConsole ACSStartContainerPy ACSEventAdmin
PY_PACKAGES = Acspy acsncImpl 

SCRIPTS_L = 

SCRIPTS   = acseventbrowser acspyInteractiveContainer acsLoggingMonitor

# 
# IDL Files and flags
# 
IDL_FILES = 
IDL_TAO_FLAGS =
USER_IDL =



#
# other files to be installed
#----------------------------
INSTALL_FILES =

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
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) *~ ../*~ Acspy/*~ acsncImpl/*~ ACSImpl/*~
	$(RM) Acspy/Util/*~ Acspy/Common/*~ Acspy/Nc/*~ Acspy/Time/*~ Acspy/Clients/*~ Acspy/Servants/*~
	$(RM) *.pyc Acspy/*.pyc acsncImpl/*.pyc ACSImpl/*.pyc
	$(RM) Acspy/Util/*.pyc Acspy/Common/*.pyc Acspy/Nc/*.pyc Acspy/Time/*.pyc Acspy/Clients/*.pyc Acspy/Servants/*.pyc
	$(RM) ../doc/*.html ../doc/pychecker
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"


#___oOo___

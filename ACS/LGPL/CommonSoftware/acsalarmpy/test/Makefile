#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2008 
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
# "@(#) $Id: Makefile,v 1.2 2010/07/22 13:33:15 jagonzal Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-10-08  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

#
# Python stuff (public and local)
# ----------------------------
PY_SCRIPTS_L       = test_Acsalarmpy \
                     test_Acsalarmpy_Timestamp.py \
                     test_Acsalarmpy_ASI.py \
                     test_Acsalarmpy_FaultState.py \
                     test_Acsalarmpy_AlarmSystemInterface.py \
                     test_Acsalarmpy_ACSAlarmSystemInterfaceProxy.py

PY_PACKAGES_L      = UnitTests


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
	@mkdir -p tmp
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) *~ *.pyc UnitTests/*~ UnitTests/*.pyc
	$(RM) tatlogs
	$(RM) tmp
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"


#___oOo___

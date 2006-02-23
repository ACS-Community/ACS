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
# "@(#) $Id: Makefile,v 1.4 2003/01/24 10:58:17 gchiozzi Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# hsommer  26/11/02  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

# 
# IDL Files and flags
# 
IDL_FILES = HelloDemo XmlComponent LampAccess LampCallback ErrorSystemComponent EventComponent
IDL_TAO_FLAGS =
USER_IDL =

ACSERRDEF = JContExmplErrTypeTest ErrorSystemExample

#
# Jarfiles: We must distinguish between component implementation classes and other classes.
#
COMPONENTS_JARFILES = jcontexmplComp
jcontexmplComp_DIRS = \
	alma/demo/ErrorSystemComp \
	alma/demo/EventConsumerImpl \
	alma/demo/EventSupplierImpl \
	alma/demo/HelloDemoImpl \
	alma/demo/LampAccessImpl \
	alma/demo/LampCallbackImpl \
	alma/demo/XmlComponentImpl

JARFILES = jcontexmplClient
jcontexmplClient_DIRS = alma/demo/client alma/demo/dyncomp alma/demo/EventConsumerImpl


DEBUG = on
#COMPONENT_HELPERS=on
XML_IDL="ObsProposal=alma.xmljbind.test.obsproposal.ObsProposal; \
         SchedBlock=alma.xmljbind.test.schedblock.SchedBlock"

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         =
SCRIPTS_L       =

#>>>>> END OF standard rules

#
# INCLUDE STANDARDS
# -----------------
ifdef ACSROOT
    MAKEDIR  = $(shell if [ -f $(INTROOT)/include/acsMakefile ];  then \
                     echo $(INTROOT)/include; \
	           else \
	             echo $(ACSROOT)/include; \
		   fi;)
    include $(MAKEDIR)/acsMakefile
else
    MAKEDIR  = $(shell if [ -f $(INTROOT)/include/vltMakefile ];  then \
                     echo $(INTROOT)/include; \
	           else \
	             echo $(VLTROOT)/include; \
		   fi;)
    include $(MAKEDIR)/vltMakefile
endif

#
# TARGETS
# -------

all:	do_all
	@echo " . . . 'all' done" 

clean : clean_all 
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	cp mainDoxyPage.html ../doc/html/main.html
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#___oOo___



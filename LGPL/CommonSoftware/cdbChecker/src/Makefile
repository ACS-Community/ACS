#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) European Southern Observatory, 2005 
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
# "@(#) $Id: Makefile,v 1.5 2011/05/24 08:19:28 rtobar Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# raraya  2005-03-25  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------


#
# Jarfiles and their directories
#
JARFILES=cdbChecker 
cdbChecker_DIRS = cl

#
## This will have to be replaced by:
# JAVA_ENDORSED=1
# # when implemented
# #
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
CLASSPATH := $(XERCES_JAR)$(PATH_SEP)$(CLASSPATH)

#
# java sources in Jarfile on/off
DEBUG = on 

#
# ACS XmlIdl generation on/off
#
XML_IDL= 
#
# Java Component Helper Classes generation on/off
#
COMPONENT_HELPERS=
#
# Java Entity Classes generation on/off
#
XSDBIND=
#
# Schema Config files for the above
#
XSDBIND_INCLUDE=

INSTALL_FILES= 	../lib/java-getopt-1.0.12.jar ../config/reqSchemas.xml \
		../config/reqSchemas.xsd

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = cdbChecker

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
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"


#___oOo___

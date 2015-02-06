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
# "@(#) $Id$"
#
# Makefile of extjars
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi 2003-06-05 Do not install any more openorb jars colliding with JacORB
# dvitas  08/05/03  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

#
# jar files to be installed
#----------------------------
INSTALL_FILES = ../lib/activation.jar \
	../lib/commons-beanutils-1.8.3.jar \
	../lib/commons-cli-1.2.jar \
	../lib/commons-collections-3.2.1.jar \
	../lib/commons-digester-2.1.jar \
	../lib/commons-jexl-1.1.jar \
	../lib/commons-lang-2.5.jar \
	../lib/commons-logging-1.1.1.jar \
	../lib/commons-math-2.1.jar \
	../lib/commons-xml-resolver-1.2.jar \
	../lib/dom4j-1.6.1.jar \
	../lib/ehcache-core-2.3.1.jar \
	../lib/ejb3-persistence.jar \
	../lib/gnu-regexp-1.1.4.jar \
	../lib/hamcrest-core-1.3.jar \
	../lib/hamcrest-library-1.3.jar \
	../lib/hsqldb.jar \
	../lib/icons.jar \
	../lib/infobus.jar \
	../lib/jakarta-oro-2.0.5.jar \
	../lib/jakarta-regexp-1.2.jar \
	../lib/javassist-3.4.GA.jar \
	../lib/jaxen-1.1.3.jar \
	../lib/jdom.jar \
	../lib/jlibeps-0.1.jar \
	../lib/jnlp.jar \
	../lib/jta-1.1.jar \
	../lib/junit-dep-4.10.jar \
	../lib/log4j-1.2.15.jar \
	../lib/mockito-core-1.9.5.jar \
	../lib/objenesis-1.2.jar \
	../lib/prevayler-1.02.001.jar \
	../lib/saxpath.jar \
	../lib/servlet-api-3.0.jar \
	../lib/slf4j-api-1.7.5.jar \
	../lib/sqltool.jar \
	../lib/tablelayout.jar \
	../lib/xalan.jar \
	../lib/xalan_serializer.jar \
	../lib/xmlunit-1.3.jar \
	../lib/mysql-connector-java-5.1.20-bin.jar

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
all:	copy_slf4japi_from_jacorb do_all
	@echo " . . . 'all' done"

copy_slf4japi_from_jacorb :
	@echo "== Copying slf4j-api-1.7.5.jar used for building jacorb to this module."
	cp -a $(JACORB_HOME)/lib/slf4j-api-1.7.5.jar ../lib/slf4j-api-1.7.5.jar

clean : clean_all
	@rm -f ../lib/slf4j-api-1.7.5.jar
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all
	@echo " . . . clean_dist done"

man   : do_man
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"



#___oOo___

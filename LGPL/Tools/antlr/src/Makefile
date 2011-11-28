#*******************************************************************************
# ALMA - Atacama Large Millimeter Array
# Copyright (c) ESO - European Southern Observatory, 2011
# (in the framework of the ALMA collaboration).
# All rights reserved.
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#*******************************************************************************
#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.7 2011/11/28 16:22:27 javarias Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# mcaillat  03/07/06  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    This makefile builds and installs ANTLR . Please note that only the
#    C++ library and include files directory and the java jar file are built
#    and installed.
#------------------------------------------------------------------------

SHELL=/bin/bash
PLATFORM := $(shell uname)

# HSO 2010-10: We currently support antrl versions 2 and 3. 
# Version 3.0.1 is needed for oAW v.5, but only for java, which is all we install. The currently latest version 3.2 does not work. 
# Since the build from source requires maven, we currently just install the pre-built jar file. 
# Note that the java packages are unique so that no version conflict can arise. 
# TODO: Check who all still uses version 2, and especially if C++ support is still needed.
#       Try to build also ANTLR_VER_3 from source.
ANTLR_VER_2 = 2.7.7
ANTLR_VER_3 = 3.0.1

#
# Scripts (public and local)
# ----------------------------
#SCRIPTS_L         = 


INSTALL_FILES = ../lib/antlr-${ANTLR_VER_3}.jar


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
all : do_all unpack configure build_antlr
	@echo " . . . 'all' done"

clean : clean_all
	@echo " . . . 'clean' done"

clean_dist : clean_all clean_dist_all
	@echo " . . . removing the antlr distribution directory . . . "
	@rm -rf antlr-${ANTLR_VER_2}
#	@rm -rf antlr-${ANTLR_VER_3}
	@echo " . . . 'clean_dist' done . . . "

clean_install : 
	@echo " . . . removing the antlr installed files . . ."
	@rm -rf ${INSTALL_ROOT}/bin/antlr-config
	@rm -rf ${INSTALL_ROOT}/bin/antlr
	@rm -rf ${INSTALL_ROOT}/lib/libantlr.a
	@rm -rf ${INSTALL_ROOT}/lib/antlr.jar
	@rm -rf ${INSTALL_ROOT}/lib/antlr-${ANTLR_VER_3}.jar
	@rm -rf ${INSTALL_ROOT}/include/antlr
	@rm -rf ${INSTALL_ROOT}/share/antlr-${ANTLR_VER_2}
	@rm -rf ${INSTALL_ROOT}/share/doc
	@echo " . . . 'clean_install' done . . . "

man :
	@echo " . . . man page(s) done . . . "

install : install_all
	@echo " . . . installing antlr C++ library and include files . . . "
	@cd antlr-${ANTLR_VER_2}; make SUBDIRS="antlr lib" install
	@echo " . . . 'install' done . . . "

.PHONY:unpack
unpack :
	@echo " . . . unpacking antlr . . . "
	@gtar -xzf antlr-${ANTLR_VER_2}.tar.gz
	@patch -p0 < gcc44.patch
#	@gtar -xzf antlr-${ANTLR_VER_3}.tar.gz

.PHONY: configure
configure : unpack
	@echo " . . . configure . . . "
	@echo "cwd is : ${CURDIR}"
	cd antlr-${ANTLR_VER_2}; ./configure --prefix=${INSTALL_ROOT} --disable-csharp --disable-python

.PHONY: build_antlr
build_antlr: configure
	@echo " . . . building antlr . . . "
	@cd antlr-${ANTLR_VER_2}; $(MAKE) $(MAKE_PARS) SUBDIRS="antlr lib"

#___oOo___

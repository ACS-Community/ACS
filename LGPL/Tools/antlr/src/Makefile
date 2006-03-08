#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.1 2006/03/08 09:16:32 mcaillat Exp $"
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
ANTLR_VER = 2.7.6

#
# Scripts (public and local)
# ----------------------------
#SCRIPTS_L         = 

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
	@rm -rf antlr-${ANTLR_VER}
	@echo " . . . 'clean_dist' done . . . "

clean_install : 
	@echo " . . . removing the antlr installed files . . ."
	@rm -rf ${INSTALL_ROOT}/lib/libantlr.a
	@rm -rf ${INSTALL_ROOT}/lib/antlr.jar
	@rm -rf ${INSTALL_ROOT}/include/antlr
	@rm -rf ${INSTALL_ROOT}/share/antlr-2.7.6
	@rm -rf ${INSTALL_ROOT}/share/doc
	@echo " . . . 'clean_install' done . . . "

man :
	@echo " . . . man page(s) done . . . "

install : 
	@echo " . . . installing antlr C++ library and include files . . . "
	@cd antlr-${ANTLR_VER}; make SUBDIRS="antlr lib" install
	@echo " . . . 'install' done . . . "

unpack :
	@echo " . . . unpacking antlr . . . "
	@gtar -xzf antlr-${ANTLR_VER}.tar.gz


configure :
	@echo " . . . configure . . . "
	@echo "cwd is : ${CURDIR}"
	cd antlr-${ANTLR_VER}; ./configure --prefix=${INSTALL_ROOT} --disable-csharp --disable-python

build_antlr :
	@echo " . . . building antlr . . . "
	@cd antlr-${ANTLR_VER}; make SUBDIRS="antlr lib"

#___oOo___

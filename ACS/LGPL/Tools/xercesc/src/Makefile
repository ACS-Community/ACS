#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.5 2010/03/03 13:18:09 mzampare Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# almamgr  12/12/03  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

SHELL=/bin/bash
PLATFORM := $(shell uname)
XERCES_VER = src_2_8_0
XERCES_MAJOR_VER = 28
export XERCESCROOT=$(CURDIR)/xerces-c-$(XERCES_VER)


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
all:	do_all unpack configure build_xerces
	@echo " . . . 'all' done" 

clean : clean_all 
	@echo " . . . clean done";\

clean_dist : clean_all clean_dist_all 
	@echo "\n . . . removing the xerces-c directory and log file . . ."
	@rm -rf xerces-c-$(XERCES_VER) configure.log
	@echo " . . . clean_dist done"

clean_install:
	@rm -rf $(INSTALL_ROOT)/lib/libxerces-c.so
	@rm -rf $(INSTALL_ROOT)/lib/libxerces-c.so.$(XERCES_MAJOR_VER)
	@rm -rf $(INSTALL_ROOT)/lib/libxerces-c.so.$(XERCES_MAJOR_VER).0
	@rm -rf $(INSTALL_ROOT)/lib/libxerces-depdom.so
	@rm -rf $(INSTALL_ROOT)/lib/libxerces-depdom.so.$(XERCES_MAJOR_VER)
	@rm -rf $(INSTALL_ROOT)/lib/libxerces-depdom.so.$(XERCES_MAJOR_VER).0
	@rm -rf $(INSTALL_ROOT)/include/xercesc
	@echo " . . . clean_install done"

man   : do_man 
	@echo " . . . man page(s) done"

install : clean_install install_all
	@echo ". . . installing xerces-c . . . "
	@cd xerces-c-$(XERCES_VER)/src/xercesc; make install
	@echo " . . . installation done"

unpack :
	@echo ". . . unpacking the tar file . . . "
	@gtar -xzf xerces-c-$(XERCES_VER).tar.gz

configure : unpack
	@echo ". . . running configure . . . "
	@echo "cwd is: $(CURDIR)"
	@ cd xerces-c-$(XERCES_VER)/src/xercesc; autoconf
	@ cd xerces-c-$(XERCES_VER)/src/xercesc; \
	./runConfigure -P $(INSTALL_ROOT) -plinux -cgcc -xg++ -minmem -nsocket -tnative -rpthread > configure.log 2>&1

.NOTPARALLE: build_xerces
build_xerces : configure
	@echo ". . . building xerces. . . "
	@cd xerces-c-$(XERCES_VER)/src/xercesc; make -j 1
#___oOo___

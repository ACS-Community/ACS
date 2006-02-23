#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.11 2005/12/01 12:00:51 gchiozzi Exp $"
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
CPPUNIT_VER = 1.10.2
CPPUNIT_TAR = 1.10.2

#
# Scripts (public and local)
# ----------------------------
SCRIPTS_L         = configure_script

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
all:	do_all unpack patch doxyfile configure build
	@echo " . . . 'all' done" 

clean : clean_all 
ifneq ($(PLATFORM),Linux)
	-$(AT) if [ -d cppunit-$(CPPUNIT_VER) ]; then cd cppunit-$(CPPUNIT_VER) ; make clean > /dev/null ; fi
endif
	@echo " . . . clean done";\

clean_dist : clean_all clean_dist_all 
	@echo "\n . . . removing the cppunit directory and log file . . ."
	@rm -rf cppunit-$(CPPUNIT_VER) configure_script.log last_patched
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo ". . . installing cppunit . . . "
	@cd cppunit-$(CPPUNIT_VER); make install
	@echo " . . . installation done"

unpack :
	@ if [ ! -d cppunit-$(CPPUNIT_VER) ]; \
          then \
            echo ". . . unpacking the tar files . . . "; \
            rm -rf last_patched; \
            gtar -xzf cppunit-$(CPPUNIT_TAR).tar.gz; \
          fi

doxyfile :
	@echo ". . . updating Doxyfile (ingnore error messages here) . . . "
	@cd cppunit-$(CPPUNIT_VER)/doc; doxygen -u Doxyfile.in

patch: last_patched

last_patched: cppunit-gcc-3_4_3.patch
	@cd cppunit-$(CPPUNIT_VER); patch -p1 < ../cppunit-gcc-3_4_3.patch
	@touch last_patched
	@echo " . . . patch applied";\

preparePatch:
	mv cppunit-gcc-3_4_3.patch cppunit-gcc-3_4_3.patch.old
	rm -rf tmp_unpack; mkdir -p tmp_unpack
	cd tmp_unpack; gtar -xzf ../cppunit-$(CPPUNIT_VER).tar.gz; mv cppunit-$(CPPUNIT_VER) cppunit-$(CPPUNIT_VER).orig
	LC_ALL=C TZ=UTC0 diff -Naur tmp_unpack/cppunit-$(CPPUNIT_VER).orig cppunit-$(CPPUNIT_VER)  >cppunit-gcc-3_4_3.patch; true
	rm -rf tmp_unpack
	@echo " . . . patch file prepared"

configure :
	@echo ". . . running configure . . . "
	@../bin/configure_script cppunit-$(CPPUNIT_TAR).tar.gz $(CPPUNIT_VER) > configure_script.log 2>&1

build :
	@echo ". . . building . . . "
	@cd cppunit-$(CPPUNIT_VER); make all
#___oOo___

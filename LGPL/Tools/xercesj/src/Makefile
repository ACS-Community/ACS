#*******************************************************************************
#
# "@(#) $Id: Makefile,v 1.9 2010/06/04 14:02:31 mzampare Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# hsommer  18/02/05  created
#

#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

XERCESJ_VER = 2.9.1
XERCESJ_DIR = xerces-2_9_1


#
# The built jar file needs to be installed
# As well as a couple of DTDs uses by the cdbChecker.
#----------------------------
INSTALL_FILES = ../lib/endorsed/xercesImpl.jar \
                ../config/CDB/schemas/datatypes.dtd \
                ../config/CDB/schemas/XMLSchema.dtd 

#
# Configuration Database Schema Files
# ----------------------
#
# We keep here a local copy of the original xml.xsd
# from the W3 org to be used by the CDB.
# In this way we do not need to access the internat when we need to
# access it.
# xerces is the most basic module that handles schema files and
# therefore we have decided to put it here.
#
CDB_SCHEMAS = xml XMLSchema

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
.NOTPARALLE: all
all:	do_all unpack patch buildXerces  
	@echo " . . . 'all' done" 

clean : clean_all
	rm -rf last_patched
	rm -rf $(XERCESJ_DIR)
	rm -rf ../lib/endorsed/xercesImpl.jar
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#
# This target prepares the patch file
# after new patches have been applied/coded.
# It assumes that the new/patched files are in
# in 
#     $(XERCESJ_DIR)
# and unpacks the unpatched code to makethe diff
#     tmp_unpack/$(XERCESJ_DIR).orig
#
# Does not use $(XERCESJ_DIR) as directory name but adds .orig
# to make clearer reading the patch file.
# Before preparing the patch also cleans up the code with the patches
# Makes a copy of the previous patch file for comparison
# and deleted the unpatched code afterwards.
# 
# I had to put a 'true' because patch returns -1. No idea why.  
#
.NOTPARALLEL: preparePatch
preparePatch:
	mv xml-xerces.patch xml-xerces.patch.old
	rm -rf tmp_unpack; mkdir -p tmp_unpack
	cd tmp_unpack; gtar -xzf ../Xerces-J-src.$(XERCESJ_VER).tar.gz; mv $(XERCESJ_DIR) $(XERCESJ_DIR).orig
	cd $(XERCESJ_DIR)/java; ./build.sh clean
	LC_ALL=C TZ=UTC0 diff -Naur tmp_unpack/$(XERCESJ_DIR).orig $(XERCESJ_DIR)  >xml-xerces.patch; true
	rm -rf tmp_unpack
	@echo " . . . patch file prepared"


#
# Unpack the tar file with the original distribution
#
.NOTPARALLE: unpack
unpack:
	@if [ ! -d $(XERCESJ_DIR) ]; then \
		echo " . . . unpacking original distribution Xerces-J-src.$(XERCESJ_VER).tar.gz";\
		gtar -xzf Xerces-J-src.$(XERCESJ_VER).tar.gz; \
	fi

#
# Apply the xpointer patch.
# Delete Xerces-J java source files that should be excluded from the build (see ICT-1178).
# Copy selected external libraries from ACSROOT and the xerces tools to the location expected by the Xerces-J build.
#
# TODOs: 
# (a) See http://ictjira.alma.cl/browse/ICT-1174 about still hardcoding $ACSROOT instead of using acsGetSpecificJars.py
# (b) Find a way of calling xercesj's build.xml so that we skip the extraction of tools/xml-commons-external-src.zip.
#     Currently we inject the empty xml-commons-external-src-dummy.zip file because it seems easier than patching build.xml.
#
patch: last_patched

last_patched: xml-xerces.patch
	@cd $(XERCESJ_DIR); patch -p1 < ../xml-xerces.patch
	@echo " . . . patch applied";\

	@rm -rf $(XERCESJ_DIR)/src/org/apache/html $(XERCESJ_DIR)/src/org/w3c/dom/html
	@echo " . . . removed HTML DOM support";\

	@cd $(XERCESJ_DIR); mkdir -p tools/bin
	@cp $(ACSROOT)/lib/commons-xml-resolver-1.2.jar $(XERCESJ_DIR)/tools/resolver.jar
	@cp xjavac.jar $(XERCESJ_DIR)/tools/bin
	@cp xml-commons-external-src-dummy.zip $(XERCESJ_DIR)/tools/xml-commons-external-src.zip
	@echo " . . . Fixed external dependencies";\

	@touch last_patched

# Build the distribution using the standard procedure.
# Copy xercesImpl.jar to ../lib/endorsed, from where it can be installed.
#
buildXerces:
	@cd $(XERCESJ_DIR); ant jar
	@mkdir -p ../lib/endorsed
	@cp $(XERCESJ_DIR)/build/xercesImpl.jar ../lib/endorsed

#___oOo___

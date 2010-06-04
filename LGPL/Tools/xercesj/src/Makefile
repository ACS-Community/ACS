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
	rm -rf xml-xerces
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
#     xml-xerces
# and unpacks the unpatched code to makethe diff
#     tmp_unpack/xml-xerces.orig
#
# Does not use xml-xerces as directory name but adds .orig
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
	cd tmp_unpack; gtar -xzf ../xml-xerces-java.tar.gz; mv xml-xerces xml-xerces.orig
	cd xml-xerces/java; ./build.sh clean
	LC_ALL=C TZ=UTC0 diff -Naur tmp_unpack/xml-xerces.orig xml-xerces  >xml-xerces.patch; true
	rm -rf tmp_unpack
	@echo " . . . patch file prepared"

#
# Unpack the tar file with the original distribution
#
.NOTPARALLE: unpack
unpack:
	@if [ ! -d xml-xerces ]; then \
	      echo " . . . unpack original distribution";\
	      gtar -xzf xml-xerces-java.tar.gz; \
        fi

#
# Apply the patch
#
patch: last_patched

last_patched: xml-xerces.patch
	@cd xml-xerces; patch -p1 < ../xml-xerces.patch
	@touch last_patched
	@echo " . . . patch applied";\

#
# Build the distribution using the standard procedure.
# Then copy the jar file in ../lib/endorsed,
# from where it can be installed.
#
buildXerces:
	@cd xml-xerces/java; ./build.sh jar
	@mkdir -p ../lib/endorsed
	@cp xml-xerces/java/build/xercesImpl.jar ../lib/endorsed

#___oOo___

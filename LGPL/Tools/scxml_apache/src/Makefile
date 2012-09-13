#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id$"
#
# Makefile of ACS/LGPL/Tools/scxml_apache/src
#
# who       when      what
# --------  --------  ----------------------------------------------
# hsommer  05/09/12  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

SCXML_VER = 0.9
SCXML_DIR = commons-scxml-$(SCXML_VER)-src
SCXML_LIBDIR = $(SCXML_DIR)/target/lib
SCXML_TARBALL = $(SCXML_DIR).tar.gz
SCXML_JAR = commons-scxml-$(SCXML_VER).jar

#
# other files to be installed
#----------------------------
INSTALL_FILES = ../lib/$(SCXML_JAR)


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
all:	do_all unpack buildScxml
	@echo " . . . 'all' done" 

clean : clean_all 
	@rm -rf $(SCXML_DIR)
	@rm -f ../lib/$(SCXML_JAR)
	@rm -rf .purifydir
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"


# ---------------------------------

# We construct the classpath outside of ANT to avoid that the build.xml script will try to download the required jar files from the web.
# See "build.sysclasspath=only" below. An alternative would be to patch the build.xml file.
REMOTE_JARS_CLASSPATH = $(ACSROOT)/lib/commons-logging-1.1.1.jar:$(ACSROOT)/lib/xalan.jar:$(ACSROOT)/lib/commons-digester-2.1.jar:$(ACSROOT)/lib/commons-beanutils-1.8.3.jar:$(ACSROOT)/lib/commons-jexl-1.1.jar

# For the local path we need a few jars that do not need to be installed though. 
# Uncomment the next 3 lines to include all of these jars, whereas now we just list them explicitly.
#empty :=
#space := $(empty) $(empty)
#LOCAL_JARS_CLASSPATH = $(subst $(space),:,$(abspath $(wildcard ../lib/*.jar)))
LOCAL_JARS_CLASSPATH = $(abspath ../lib/commons-el-1.0.jar):$(abspath ../lib/jsp-api-2.0.jar):$(abspath ../lib/myfaces-api-1.1.5.jar):$(abspath ../lib/servlet-api-2.4.jar)

# unpack target:
.NOTPARALLE: unpack
unpack:
	@if [ ! -d $(SCXML_DIR) ]; then \
	      echo " . . . unpack original distribution";\
	      gtar -xzf $(SCXML_TARBALL); \
	fi


# build the jar file.
.NOTPARALLE: buildScxml
buildScxml :
	@echo ". . . building apache scxml . . . "

	cd $(SCXML_DIR); CLASSPATH=$(LOCAL_JARS_CLASSPATH):$(REMOTE_JARS_CLASSPATH); ant -Dbuild.sysclasspath=only jar
	
	cp -r --preserve=timestamps $(SCXML_DIR)/src/main/java $(SCXML_DIR)/target; cd $(SCXML_DIR)/target; mv java src
	cd $(SCXML_DIR)/target; jar uf $(SCXML_JAR) src
	
	cp $(SCXML_DIR)/target/$(SCXML_JAR) ../lib

#___oOo___

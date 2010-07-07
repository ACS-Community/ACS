#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.160 2010/07/07 10:25:21 alopatin Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# dave 2003-09-04 removed startACS as it has been moved to the acsstartup module
# gchiozzi 2003-06-04 Added acsdataClean script
# gchiozzi 2003-06-02 Moved acsIrFeed in the acsutil module, since it is needed before acsBuild is built
# gchiozzi 2003-02-03 Re-written man and install targets to handle whole ACS documentation, but still an hack
# gchiozzi 2003-02-03 Removed acsBUILDInstallReadme
# gchiozzi 2003-01-09 Replaced INTROOT with VLTTOP in man target
# gchiozzi 2002-12-12 Added acsBUILDPrepareKit.sh
# almamgr 2002-11-28 Added hook for retrieve
# gchiozzi 2002-11-22 Added acsTATPrologue acsTATEpilogue
# jibsen   2002-04-10 acsDHCPBoot, acsDHCPConfig added 
# psivera  2002-03-14 Added acsBUILDAfterBuildMod.sh.
# gchiozzi 2002-01-17 Added acsBUILDTest.
# gchiozzi 2001-11-19 Removed acsAllOrbacus.idl file
# gchiozzi 2001-11-16 Added acsIrfeed
# gchiozzi 2001-09-19 Added clean_dist target
# gchiozzi 2001-09-19 Added various startup/shutdown scripts
# psivera 2001-08-31 moved the installation of config files from
#		     "make install" to "make man" section
# psivera 2001-08-30 removed acsBUILDAfterInstall.sh (obsolete)
# almamgr 2001-08-07 Added doxygen of man pages
# almamgr 2001-05-28 Renamed module acsBUILD.
# rkarban  29/02/00  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

#
# user definable C-compilation flags
#USER_CFLAGS = 

#
# additional include and library search paths
#USER_INC = 
#USER_LIB = 

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     =
EXECUTABLES_L   = 

#
# <brief description of xxxxx program>
xxxxx_OBJECTS   =	
xxxxx_LDFLAGS   =
xxxxx_LIBS      =

#
# special compilation flags for single c sources
#yyyyy_CFLAGS   = 

#
# Includes (.h) files (public only)
# ---------------------------------
INCLUDES        =

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       =
LIBRARIES_L     =

#
# <brief description of lllll library>
lllll_OBJECTS   =


DEBUG = on

#
# Jarfiles and their directories
# Currently the produced JAR file is not installed, 
# so just uncomment the following two lines to  build it manually.
# This will provide the ProcessCvs2clOutput command to generate ChangeLog files for the wiki.
#
#JARFILES = acsBUILD 
#acsBUILD_DIRS =  alma


#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = acsBUILDBeforeInstall.sh acsBUILDTest \
                  acsBUILDPrepareKit.sh \
                  acsStartLoggingClient \
                  acsDHCPBoot \
                  generateJarsFromIDLs acsBUILDAfterBuildMod.sh\
                  acsBUILDCheckModuleTree.sh \
                  acsUpdate \
                  acsCopyMidlToPidl
SCRIPTS_L       = 


PY_SCRIPTS =
PY_SCRIPTS_L = acsGenGlobalPythonIndex

#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     =
TCL_SCRIPTS_L   =

#
# <brief description of tttttt tcl-script>
tttttt_OBJECTS  =
tttttt_TCLSH    = 
tttttt_LIBS     = 

#
# TCL libraries (public and local)
# ------------------------------
TCL_LIBRARIES   =
TCL_LIBRARIES_L =

#
# <brief description of tttlll library>
tttlll_OBJECTS  = 

#
# UIF panels (public and local)
# ----------------------------
PANELS   =
PANELS_L = 

#
# man pages to be done
# --------------------
MANSECTIONS = 1
MAN1 = acsDHCPBoot acsUpdate
MAN3 =
MAN5 =
MAN7 =
MAN8 =

#
# local man pages
# ---------------
MANl = 

#
# ASCII file to be converted into Framemaker-MIF
# --------------------
ASCII_TO_MIF = 

#
# INS_ROOT files to be installed
#-------------------------------
INS_ROOT_FILES =
INS_ROOT_DIR   =

#
# other files to be installed
#----------------------------
INSTALL_FILES =

#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

#
#>>>>> END OF standard rules

#
# CHOSE PLATFORM
# --------------
# Default is UNIX, for VxVorks application next line MUST BE UNCOMMENTED
#MAKE_VXWORKS = on   

#
# INCLUDE STANDARDS
# -----------------
SHELL = /bin/sh 
MAKEDIRTMP := $(shell searchFile include/acsMakefile)
ifneq ($(MAKEDIRTMP),\#error\#)
   MAKEDIR := $(MAKEDIRTMP)/include
   include $(MAKEDIR)/acsMakefile
endif



#
# ACS supported TARGETS
# -------
all:	do_all
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) ../*~ ../*/*~ ../*/*~ ../*/*/*~ ../*/*/*/*~ ../*/*/*/*/*~
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo "Cleaning up documentation tree in ../.."
	@rm -rf ../../man
	@echo " . . . clean_dist done"

#
# TODO: GCH build and install of complete ACS documentation is an hack
#           and should be re-written in a nicer way
# 
man   : do_man man_prepare man_idl man_cpp man_java man_schema man_python man_errors
	@echo " . . . documentation page(s) done"
man_prepare:
	@echo "Generating complete ACS documentation in build root ../.."
	@echo "Cleaning up documentation tree"
	@rm -rf ../../man/*

man_idl:
	@echo "Generating IDL documentation"
	@cd ../..; doxygen acsBUILD/config/Doxyfile.idl
man_cpp:
	@echo "Generating C++ API documentation"
	@cd ../..; doxygen acsBUILD/config/Doxyfile.cpp
man_java:
	@echo "Generating Java API documentation"
	@cd ../..; doxygen acsBUILD/config/Doxyfile.java
	@cp ../../man/java/annotated.html ../../man/java/index.html
man_schema:
	@echo "Generating schema documentation "
	@mkdir -p $(VLTTOP)/man/schemas/src
	@cp -f $(VLTTOP)/config/CDB/schemas/*.xsd $(VLTTOP)/idl/*.xsd $(VLTTOP)/config/CDB/schemas/*.dtd $(VLTTOP)/man/schemas/src
	@cp ../config/xsddocSchemaAntBuild.xml $(VLTTOP)/man/schemas/src/build.xml
	@cd $(VLTTOP)/man/schemas/src; \
         export CLASSPATH=$(shell searchFile lib/xsddoc.jar)/lib/xsddoc.jar:$(ACSROOT)/lib/xalan.jar:$(ACSROOT)/lib/xalan_serializer.jar:$(CLASSPATH) ;\
         ant
man_python:
	@echo "Generating global Python API documentation index file"
	@mkdir -p $(VLTTOP)/man/py
	@acsGenGlobalPythonIndex $(VLTTOP)/man/py > $(VLTTOP)/man/py/index.html

man_errors:
	@echo "Generating errors documentation"
	@mkdir -p $(VLTTOP)/man/errors
	@python acsGenErrorsDocumentation.py $(VLTTOP) $(VLTTOP)/man/errors


install : install_doc install_config install_cdb install_svc_conf install_all
	@echo " . . . installation done"

install_doc:
	@echo " . . . installing ACS documentation from ../.."
	-@cp -r ../../man $(VLTTOP);\
	         chmod -R ug+rw $(VLTTOP)/man;

install_config:
	@echo " . . . installing user account configuration files ACSROOT/config"
	-@gtar -cf - --exclude="CVS" -C ../config .acs  | gtar -xf - -C $(VLTTOP)/config; \
	         chmod -R ug+rw $(VLTTOP)/config/.acs;
	@echo " . . . copying rc.d configuration files for DHCP"
	-@gtar -cf - --exclude="CVS" -C ../config rc.d  | gtar -xf - -C $(VLTTOP)/config; \
	         chmod -R ug+rw $(VLTTOP)/config/rc.d;

install_cdb:
	@echo " . . . copying default CDB in ACSDATA"
	-@gtar -cf - --exclude="CVS" -C ../config defaultCDB  | gtar -xf - -C $(ACSDATA)/config; \
	         chmod -R ug+rw $(ACSDATA)/config/defaultCDB;
	@echo " . . . copying demo CDB in ACSDATA"
	-@gtar -cf - --exclude="CVS" -C ../config demoCDB  | gtar -xf - -C $(ACSDATA)/config; \
	         chmod -R ug+rw $(ACSDATA)/config/demoCDB;

install_svc_conf:
	@echo " . . . copying svc.conf files"
	-@cp -r ../config/svc.conf $(ACSROOT)/config;\
	     	 chmod -R ug+rw $(ACSROOT)/config/svc.conf;
#___oOo___

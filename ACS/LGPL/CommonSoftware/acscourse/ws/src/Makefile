#*******************************************************************************

# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: Makefile,v 1.23 2012/01/21 22:48:11 tstaig Exp $" 
#
# Makefile of ........
#
#

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# Flag to switch on debugging information
# ----------------------------
#
DEBUG=on

#
# Error definition files for exceptions
# and completions
# ----------------------------
#
ACSERRDEF = ACSErrTypeACSCourse

#
# Java sources
# ----------------------------
#
JARFILES =              acscourseImpl
acscourseImpl_DIRS   =  alma
acscourseImpl_EXTRAS =
acscourseImpl_JLIBS = acscourseMount


XSDBIND = acscourseEntities
XSDBIND_INCLUDE = systementities   

COMPONENT_HELPERS=on

# defines the Java binding class to be used for the corresponding IDL typedefs
XML_IDL="MyXmlConfigData=alma.acscourse.xmlbinding.myxmlconfigdata.MyXmlConfigData"

#
# CPP sources
# ----------------------------
#
USER_CFLAGS =
USER_LIB =

#
# Includes (.h) files (public and local)
# 
INCLUDES =	acscourseMount1Impl.h \
	        acscourseMount2Impl.h acscourseMount2LoopImpl.h \
		acscourseMount3Impl.h acscourseMount4Impl.h acscourseMount5Impl.h

#
# Libraries (public and local)
# 
LIBRARIES =	acscourseMount1Impl acscourseMount2Impl acscourseMount2LoopImpl acscourseMount3Impl acscourseMount4Impl acscourseMount5Impl

acscourseMount1Impl_OBJECTS	= acscourseMount1Impl
acscourseMount1Impl_LIBS	= acscourseMountStubs xmlentityStubs ACSErrTypeACSCourse acscomponent archiveevents

acscourseMount2Impl_OBJECTS	= acscourseMount2Impl acscourseMount2ImplDLL
acscourseMount2Impl_LIBS	= acscourseMountStubs xmlentityStubs ACSErrTypeACSCourse acscomponent baci archiveevents

acscourseMount2LoopImpl_OBJECTS	= acscourseMount2LoopImpl acscourseMount2Impl
acscourseMount2LoopImpl_LIBS	= acscourseMountStubs xmlentityStubs ACSErrTypeACSCourse acscomponent baci archiveevents

acscourseMount3Impl_OBJECTS	= acscourseMount3Impl 
acscourseMount3Impl_LIBS	= acscourseMountStubs xmlentityStubs ACSErrTypeACSCourse acscomponent baci archiveevents

acscourseMount4Impl_OBJECTS	= acscourseMount4Impl
acscourseMount4Impl_LIBS	= acscourseMountStubs xmlentityStubs ACSErrTypeACSCourse acscomponent baci archiveevents

acscourseMount5Impl_OBJECTS	= acscourseMount5Impl
acscourseMount5Impl_LIBS	= acscourseMountStubs xmlentityStubs acsnc ACSErrTypeACSCourse acscomponent baci archiveevents
#
# Configuration database schema Files
# ------------------------------------
CDB_SCHEMAS = ACSCourseMount ACSCourseMountRW

#
# IDL FILES
#----------------------------
IDL_FILES = 	acscourseMount
USER_IDL = 
acscourseMountStubs_LIBS = baciStubs ACSErrTypeACSCourse ACSErrTypeCommon xmlentityStubs

#
# Python sources
#----------------------------
PY_SCRIPTS = mountSimple mountCallback acscourseMountSupplier acscourseMountConsumer

PY_PACKAGES = ACSCOURSE_MOUNTImpl


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

#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))


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
	$(RM) *~ ../include/*~ ../idl/*~ ../*~ ../../*~ ../doc/html/* core
	$(RM) tmp.txt 
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : 
	@echo " . . . no installation to be done for this test module"

#___oOo___

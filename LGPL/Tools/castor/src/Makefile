#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id: Makefile,v 1.11 2010/08/16 12:33:37 mzampare Exp $"
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
# Scripts (public and local)
# ----------------------------
SCRIPTS         =
SCRIPTS_L       =

#
# Python stuff (public and local)
# ----------------------------
PY_SCRIPTS         =
PY_SCRIPTS_L       =

#
# Jarfiles and their directories
#
#

# This will have to be replaces by:
#   JAVA_ENDORSED=1
# when implemented
# See SPR ALMASW2004064
#
#XERCES_JARTMP := $(shell searchFile lib/endorsed/xercesImpl.jar)
#ifneq ($(XERCES_JARTMP),\#error\#)
#   XERCES_JAR := $(XERCES_JARTMP)/lib/endorsed/xercesImpl.jar
#endif
#
#CLASSPATH := $(CLASSPATH):$(XERCES_JAR)

# We have to append ant.jar that castor builds @ Sun
# it seems that therre is problem with automatic appending of ant.jar by ant
# it might be that script $ANT_HOME/bin/ant does not work properly @ Sun
CLASSPATH := $(CLASSPATH):$(ANT_HOME)/lib/ant.jar

JARFILES=castor
castor_DIRS=org
castor_EXTRAS=org/exolab/castor/castor.properties \
        org/exolab/castor/builder/castorbuilder.properties \
        org/exolab/castor/util/resources/messages.properties \
        org/exolab/castor/util/resources/SimpleTypes.properties \
        org/exolab/castor/util/resources/SimpleTypesMapping.properties
castor_ENDORSED=on

#
# java sources in Jarfile on/off
DEBUG=on

#
# other files to be installed
#----------------------------
INSTALL_FILES =

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
.NOTPARALLEL: all
all:	preAnt do_all 
#all:	do_all 
	@echo " . . . 'all' done" 

clean : clean_all cleanAnt
#clean : clean_all 
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

RUN_ANT_PRE = resurrectPatchedSource

cleanAnt:
	ant clean

org:
	$(AT)export CLASSPATH=""; ant -DDEBUG=$(DEBUG) \
	-DINTROOT=$(INTROOT) -DACSROOT=$(ACSROOT) \
	resurrectPatchedSource

preAnt:
	$(AT)for member in $(foreach name, $(RUN_ANT_PRE), $(name) ); \
		do \
		    export CLASSPATH=""; ant -DDEBUG=$(DEBUG) -DINTROOT=$(INTROOT) -DACSROOT=$(ACSROOT) $${member} ;\
		done


#___oOo___

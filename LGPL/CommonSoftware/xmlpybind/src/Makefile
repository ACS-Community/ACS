#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id: Makefile,v 1.1 2009/12/18 22:47:32 agrimstrup Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  15/12/09  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

SCRIPTS		= generateXsdPythonBinding

#
# Python stuff (public and local)
# ----------------------------
PY_SCRIPTS         = 
PY_SCRIPTS_L       =

PY_MODULES         =
PY_MODULES_L       =

PY_PACKAGES        = xmlpybind
PY_PACKAGES_L      =

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

Schemata:
	pyxbgen --module-prefix=xmlpybind -u ../../xmljbind/src/alma/tools/entitybuilder/EntitybuilderSettings.xsd -m EntitybuilderSettings

all:	Schemata do_all
	@echo " . . . 'all' done" 

clean : clean_all 
	@rm -rf ./xmlpybind
	@rm -f *~
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"


#___oOo___

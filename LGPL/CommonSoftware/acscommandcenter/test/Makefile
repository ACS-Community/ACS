


# who        when       what
# --------  ----------  ----------------------------------------------
# mschilli 2003-11-05  created


#******************************************************************************
# This Makefile follows ALMA Standards (see Makefile(5) for more).
#******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

# 
# IDL Files and flags
# 
IDL_FILES = 
IDL_TAO_FLAGS =
USER_IDL =
#
# Jarfiles and their directories
#
JARFILES = commandcenterTest
commandcenterTest_DIRS = alma

DEBUG = on

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = 
SCRIPTS_L       = prologue \
		  commandcenterTest \
		  epilogue

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

all:	do_all
	@echo " . . . 'all' done" 

clean : clean_all
	@echo "Deleting logs in scriptlogs directory..." 
	-@rm -rf scriptlogs/run*
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

db : db_all
	@echo " . . . ../DB done"


#___oOo___

#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.77 2004/09/22 07:14:51 mpasquat Exp $"
#
# Makefile of ...<lcuenv>... environment
#
# who       when      what
# --------  --------  ----------------------------------------------
# NNNNNNNN  dd/mm/yy  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

#
# On-Line Database Files
# ----------------------
DBL_BCF= BRANCH USER DATABASE
DBL_FLAGS = -v0

#
#>>>>> END OF standard rules

#
# CHOSE PLATFORM
# --------------
# Default is UNIX, for VxVorks application next line MUST BE UNCOMMENTED
MAKE_VXWORKS = on  

#
# INCLUDE STANDARDS
# -----------------
MAKEDIRTMP := $(shell searchFile include/vltMakefile)
ifneq ($(MAKEDIRTMP),\#error\#)
   MAKEDIR := $(MAKEDIRTMP)/include
   include $(MAKEDIR)/vltMakefile
endif


#
# TARGETS
# -------
db : db_all
	@echo " . . . ../DB done"

clean : clean_all 
	@echo " . . . clean done"

#___oOo___

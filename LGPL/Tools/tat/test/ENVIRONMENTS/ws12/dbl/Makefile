#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.77 2004/09/22 07:15:35 mpasquat Exp $"
#
# Makefile of ws (tat12) environment
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
DBL_CLASSES = 
DBL_BCF = TAT12 USER DATABASE
DBL_LOAD = DATABASE
DBL_FLAGS = -v0
xxxx_DBL_FLAGS =

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

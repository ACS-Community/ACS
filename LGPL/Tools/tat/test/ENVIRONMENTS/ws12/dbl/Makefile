#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.78 2011/10/11 13:21:16 psivera Exp $"
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
MAKEDIR = $(VLTROOT)/include
include $(MAKEDIR)/vltMakefile

#
# TARGETS
# -------
db : db_all
	@echo " . . . ../DB done"

clean : clean_all 
	@echo " . . . clean done"



#___oOo___

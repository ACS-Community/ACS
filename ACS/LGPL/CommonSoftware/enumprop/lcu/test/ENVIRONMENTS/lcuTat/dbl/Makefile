#******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.29 2004/09/16 09:26:10 mpasquat Exp $"
#
# Makefile of ...<lcuenv>... environment
#
# who       when      what
# --------  --------  ----------------------------------------------
# NNNNNNNN  dd/mm/yy  created
#

#******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

#
# On-Line Database Files
# ----------------------
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
MAKEDIRTMP := $(shell searchFile include/acsMakefile)
ifneq ($(MAKEDIRTMP),\#error\#)
   MAKEDIR := $(MAKEDIRTMP)/include
   include $(MAKEDIR)/acsMakefile
endif


#
# TARGETS
# -------
db : db_all
	rm -f ../lccSnapshot.db
	@echo "removing old snapshot file ../lccSnapshot.db"
	@echo " . . . ../DB done"

clean : clean_all 
	@echo " . . . clean done"

#___oOo___

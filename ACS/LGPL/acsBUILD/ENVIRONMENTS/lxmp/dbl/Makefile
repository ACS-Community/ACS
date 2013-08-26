#******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.123 2004/09/16 16:20:51 mpasquat Exp $"
#
# Makefile of lt1hb environment
#
# who        when       what
# -------- -------- ----------------------------------------------
# kwirenst 30/04/97 Added target 'db_install' 
# ssandroc 22/03/96 created
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
# DBL_FLAGS = -v0 -DTARGET_TCS=${TARGET_TCS} -DTCSID=$(TCSID) -I$(VLTTOP)/vw/dbl -I$(VLTTOP)/vw/include -I$(ACSROOT)/dbl -I$(ACSROOT)/vw/dbl
DBL_FLAGS = -v0 -I$(VLTTOP)/vw/dbl -I$(VLTTOP)/vw/include -I$(ACSROOT)/dbl -I$(ACSROOT)/vw/dbl

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
db : db_all db_install
	@echo " . . . ../DB done"

clean : clean_all 
	@echo " . . . clean done"

db_install:
	-$(AT)if [ -f ../lccSnapshot.db ]; \
	then \
		echo "==Delete current ../lccSnapshot.db"; \
		rm ../lccSnapshot.db;  \
	fi


#___oOo___


#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id: Makefile.c++,v 1.2 2010/07/27 23:05:50 jagonzal Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# bjeram  17/04/08  created
#

#*******************************************************************************
# REMARKS
#     Here we generate stubs and skelts for C++ only
#------------------------------------------------------------------------

MAKE_NOIFR_CHECK = on # jagonzal: there is a cyclic dependency between acsstartupIrFeed and acsstartupLoadIFR (checker)

MAKE_ONLY=C++

#
# user definable C-compilation flags
#USER_CFLAGS = 

#
# additional include and library search paths
#USER_INC = 
#USER_LIB = 

# 
# IDL Files and flags
# 
IDL_FILES =
TAO_IDLFLAGS =
# USER_IDL is defined in top Makefile, so it has to be commented here !!
#USER_IDL =

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
all:	do_all
	@echo " . . . 'all' done" 

clean : clean_all 
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"


#___oOo___

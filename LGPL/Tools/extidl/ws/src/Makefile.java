#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id: Makefile.java,v 1.11 2010/08/10 06:59:47 mzampare Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# bjeram  17/04/08  created
#

# REMARKS
#    Here we generate stubs and skelts for Java only 
#------------------------------------------------------------------------

MAKE_NOIFR_CHECK = on # jagonzal: there is a cyclic dependency between acsstartupIrFeed and acsstartupLoadIFR (checker)

MAKE_ONLY=Java
DEBUG=on
# 
# IDL Files and flags
# 
IDL_TO_INSTALL=NotificationServiceMC NotifyExt 
IDL_FILES_L = Monitor_Types Monitor $(IDL_TO_INSTALL) NotifyMonitoringExt

#IDL_FILES =  NotifyExt 
#IDL_FILES_L = NotificationServiceMC  NotifyMonitoringExt

TAO_IDLFLAGS =
# USER_IDL is defined in top Makefile, so it has to be commented here !!
#USER_IDL =


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
all:	do_all
	@echo " . . . 'all' done" 

clean : clean_all 
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	# line below is superfluous with new acsMakefile
	# but will be needed as long as the old one is in use
	@cp $(foreach idl,$(IDL_TO_INSTALL),../idl/$(idl).idl) $(VLTTOP)/idl
	@cp $(foreach jar,$(IDL_FILES_L),../lib/$(jar).jar) $(VLTTOP)/lib

	@echo " . . . installation done"

#___oOo___

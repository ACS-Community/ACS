#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id: Makefile.java,v 1.7 2010/07/27 23:05:55 jagonzal Exp $"
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

IDL_FILES = Monitor_Types Monitor NotificationServiceMC NotifyExt NotifyMonitoringExt
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
	@cp ../idl/NotifyExt.idl $(VLTTOP)/idl
	# 
	@cp ../lib/NotifyMonitoringExt.jar $(VLTTOP)/lib
	@cp ../lib/NotificationServiceMC.jar $(VLTTOP)/lib

	@echo " . . . installation done"

#___oOo___

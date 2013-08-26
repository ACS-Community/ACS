#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id: Makefile,v 1.6 2010/06/08 10:09:16 bjeram Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# bjeram  17/04/08  created
#


export USER_IDL = -I$(ACE_ROOT)/TAO/orbsvcs -I$(ACE_ROOT)/TAO/

#
# INCLUDE STANDARDS
# -----------------

# we shall not include the acsMakefile here otherwise we have toruble with make install for python

#MAKEDIRTMP := $(shell searchFile include/acsMakefile)
#ifneq ($(MAKEDIRTMP),\#error\#)
#   MAKEDIR := $(MAKEDIRTMP)/include
#   include $(MAKEDIR)/acsMakefile
#endif


#
# TARGETS
# -------
all:	do_copy 
	@$(MAKE) -f Makefile.java  all
	@$(MAKE) -f Makefile.python  all
	@$(MAKE) -f Makefile.c++  all
	@echo " . . . 'all' done" 

clean : do_remove
	@$(MAKE) -f Makefile.java  clean
	@$(MAKE) -f Makefile.python  clean
	@$(MAKE) -f Makefile.c++  clean
	@echo " . . . clean done"

clean_dist : do_remove
	@$(MAKE) -f Makefile.java  clean_dist
	@$(MAKE) -f Makefile.python  clean_dist
	@$(MAKE) -f Makefile.c++  clean_dist
	@echo " . . . clean_dist done"

man   : 
	@$(MAKE) -f Makefile.java  man
	@$(MAKE) -f Makefile.python  man
	@$(MAKE) -f Makefile.c++  man
	@echo " . . . man page(s) done"

install : 
	@$(MAKE) -f Makefile.java  install
	@$(MAKE) -f Makefile.python install
	@$(MAKE) -f Makefile.c++  install
	@echo " . . . installation done"

# to be able to compile pidl files for Java we have to create links!
do_copy :
	@mkdir -p ../idl
	@cp -f $(ACE_ROOT)/TAO/orbsvcs/orbsvcs/NotifyExt.idl ../idl
	@cp -f $(ACE_ROOT)/TAO/orbsvcs/orbsvcs/Notify/MonitorControl/NotificationServiceMC.idl ../idl
	@cp -f $(ACE_ROOT)/TAO/orbsvcs/orbsvcs/Notify/MonitorControlExt/NotifyMonitoringExt.idl ../idl
	@cp -f $(ACE_ROOT)/TAO/orbsvcs/orbsvcs/DsLogAdmin.idl ../idl
	@cp -f $(ACE_ROOT)/TAO/tao/TimeBase.pidl ../idl
	@cp -f $(ACE_ROOT)/TAO/tao/StringSeq.pidl ../idl
	@cp -f $(ACE_ROOT)/TAO/tao/Monitor/Monitor.pidl ../idl
	@ln -fs ../idl/Monitor.pidl ../idl/Monitor.idl
	@cp -f $(ACE_ROOT)/TAO/tao/Monitor/Monitor_Types.pidl ../idl
	@ln -fs ../idl/Monitor_Types.pidl ../idl/Monitor_Types.idl

do_remove :
	@rm -rf ../idl

#___oOo___

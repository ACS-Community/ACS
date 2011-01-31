# Settings to be synchronized in ../../ws/src/Makefile and ../../lcu/src/Makefile
# Moved to a common file for inclusion 

# Version of current patched state of log4cpp package
# Version of original tgz of log4cpp package
# LOG4CPP_VER = 1.0+
# LOG4CPP_TAR = 1.0

LOG4CPP_VER = 1.0+
LOG4CPP_TAR = 1.0

#
# Check for installation directories
#
# Initially this was tested only before configuring on rule execution.
# But if the folder gets deleted in the meantime make install would not work anyway.
# Therefore for simplification, do the check on every make run.
# On INTLIST -
# Don't check existence of folders, e.g. like .bash_profile.acs
# Always use first folder
# Use builtin make expansion instead of external tools.
# INSTALL_DIR:=$(shell echo $(INTLIST) | awk 'BEGIN {FS=":"} {print $$1}')
# INSTALL_DIR:=$(firstword $(subst :, ,$(INTLIST))))
ifneq ($(INTROOT),) 
   INSTALL_DIR:=$(INTROOT)
else
ifneq ($(INTLIST),) 
   INSTALL_DIR:=$(firstword $(subst :, ,$(INTLIST)))
else
ifneq ($(ACSROOT),) 
   INSTALL_DIR:=$(ACSROOT)
else
ifneq ($(VLTROOT),) 
   INSTALL_DIR:=$(VLTROOT)
else 
   $(error No installation directories (INTROOT, INTLIST, ACSROOT, VLTROOT) have been found. Please define one and start again)
endif
endif
endif
endif


#___oOo___

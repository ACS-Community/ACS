#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.25 2004/10/05 02:59:44 srankin Exp $"
#
# Makefile to copy source-code templates into $ACSROOT/templates/forCoding/
#
# who       when        what
# --------  ----------  ----------------------------------------------
# psivera   2001-09-19  replaced ACSROOT with INSTALL_ROOT
# psivera   2000-10-16  created
#

#*******************************************************************************
# This Makefile follows ACS Standards (see Makefile manpage documentation).
#*******************************************************************************
# REMARKS
#    only "install" is meaningful   
#------------------------------------------------------------------------

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public will be cleaned and installed and 
#                     local (_L) will be only cleaned.


TEMPLATE_FILES = \
		c-main.template		\
		c-procedure.template	\
		h-file.template		\
		interface-file.template	\
		pythonModule.template	\
		pythonScript.template	\
		script.template		\
		tclScript.template	\
		RELEASE_NOTES.template  \
                c++-class-file.template \
                c++-file.template       \
                c++-h-file.template     \
                c++-small-main.template \
                dbl-file.template       \
                java-class-interface.template


FILE_LIST = 	README $(TEMPLATE_FILES)

#
# man pages to be done
# --------------------
MANSECTIONS = 5
MAN5 = $(addsuffix .doc, $(TEMPLATE_FILES))

#
# CHOSE PLATFORM
# --------------
# Default is UNIX, for VxVorks application next line MUST BE UNCOMMENTED
#MAKE_VXWORKS = on   

#
# INCLUDE STANDARDS
# -----------------
MAKEDIRTMP := $(shell searchFile include/acsMakefile)
ifneq ($(MAKEDIRTMP),\#error\#)
   MAKEDIR := $(MAKEDIRTMP)/include
   include $(MAKEDIR)/acsMakefile
endif


TEMPLATE_DIR=$(INSTALL_ROOT)/templates
TARGET_DIR=$(TEMPLATE_DIR)/forCoding

#
# TARGETS
# -------
all:
	@echo " . . . 'all' done" 

clean : clean_all
	@echo " . . . clean done"

man   : do_man
	@echo " . . . man page(s) done"

install :
	@echo "\nInstall source-code templates into $(TARGET_DIR) . . . ."
	@if [ -d $(TEMPLATE_DIR) ];\
	    then \
		 rm -fr $(TARGET_DIR)             ;\
		 mkdir  $(TARGET_DIR)             ;\
		 cp $(FILE_LIST) $(TARGET_DIR)    ;\
		 chmod -wx $(TARGET_DIR)/*        ;\
		 ls -la  $(TARGET_DIR)            ;\
		echo " . . . installation done"  ;\
	    else \
		echo "\nCannot install. (TEMPLATE_DIR) does not exist."; \
		echo " . . . installation failed"  ;\
	fi

#___oOo___

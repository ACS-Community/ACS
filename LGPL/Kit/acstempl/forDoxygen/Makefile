#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.23 2004/09/16 13:31:00 mpasquat Exp $"
#
# Makefile to copy html templates into $ACSROOT/templates/forDoxygen
#
# who       when        what
# --------  ----------  ----------------------------------------------
# mzampare  10/1/2002	first release
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
                generic-alma-doxygen \
                doxygen-header.html \
		doxygen-footer.html \
		almapic.jpg         \
		alma_logo.jpg

FILE_LIST = 	$(TEMPLATE_FILES)

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
TARGET_DIR=$(TEMPLATE_DIR)/forDoxygen

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
	@echo "\nInstall html templates into $(TARGET_DIR) . . . ."
	@if [ -d $(TEMPLATE_DIR) ];\
	    then \
		 rm -fr $(TARGET_DIR)             ;\
		 mkdir  $(TARGET_DIR)             ;\
		 cp $(FILE_LIST) $(TARGET_DIR)    ;\
		 chmod -wx $(TARGET_DIR)/*        ;\
	 	echo " . . . installation done"  ;\
	    else \
		echo "\nCannot install. (TEMPLATE_DIR) does not exist."; \
		echo " . . . installation failed"  ;\
	fi

#___oOo___

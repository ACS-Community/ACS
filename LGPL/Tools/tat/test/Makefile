#*******************************************************************************
# E.S.O. - VLT project
#
#


#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = 
SCRIPTS_L       =


#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

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
ifdef ACSROOT
    NO_TEST_TARGET := defined
    export NO_TEST_TARGET
    MAKEDIRTMP := $(shell searchFile include/acsMakefile)
    ifneq ($(MAKEDIRTMP),\#error\#)
        MAKEDIR := $(MAKEDIRTMP)/include
        include $(MAKEDIR)/acsMakefile
    endif
else
    MAKEDIR = $(VLTROOT)/include
    include $(MAKEDIR)/vltMakefile
endif

#
# TARGETS
# -------
all:	do_all
	@echo " . . . 'all' done" 

clean : clean_all 
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

ifdef ACSROOT
test :
	tat -nc -f TestList.alma
	@echo " . . . test done"
endif

#___oOo___

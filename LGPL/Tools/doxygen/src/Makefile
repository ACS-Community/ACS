#*************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.21 2005/09/30 06:55:49 mpasquat Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# psivera  13/07/01  created
# psivera  03/09/01  added graphviz stuff (in dist_clean)
#

#************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

#
# user definable C-compilation flags
#USER_CFLAGS = 
PLATFORM := $(shell uname)
DOX_VER = 1.3.6
GRAPH_VER = 2.6


#
# TARGETS
# -------
all:    unpack patch all_first
	@echo " . . . 'all' done" 
	chmod -f +x doxygen-$(DOX_VER)/addon/doxywizard/CVS \
                    doxygen-$(DOX_VER)/addon/doxywizard/obj \
                    doxygen-$(DOX_VER)/addon/mifgen/obj

all_first:
	@echo ". . . building/installing code . . . "
	@./all_install_script $(DOX_VER) $(GRAPH_VER) > all_install_script.log 2>&1 

clean : clean_dist 
	@echo " . . . 'clean' done"

# Patch because of packaging problem in doxygen tar file
# Two directories do not have execute permission and therefore I add a chmod
#
# N.B.: since at each make all the sources are unziped, in order
# to always apply the patch at each make all, the file last_patched is here
# deleted
#
clean_dist :  
	@echo " . . . removing the doxygen/graphviz directories and log file . . ."
	rm -rf doxygen-$(DOX_VER) graphviz-$(GRAPH_VER) all_install_script.log last_patched


man   : 
	@echo " . . . man page(s) done"

install : 
	@echo " . . . 'install' done"


db : 
	@echo " . . . ../DB done"

#
# This target prepares the patch file
# after new patches have been applied/coded.
# It assumes that the new/patched files are in
# in 
#     doxygen-$(DOX_VER)
# and unpacks the unpatched code to make the diff
#     tmp_unpack/doxygen-$(DOX_VER).orig
#
# Does not use doxygen as directory name but adds .orig
# to make clearer reading the patch file.
# Before preparing the patch also cleans up the code with the patches
# Makes a copy of the previous patch file for comparison
# and deleted the unpatched code afterwards.
# 
# I had to put a 'true' because patch returns -1. No idea why.  
#
preparePatch:
	mv doxygen.patch doxygen.patch.old
	rm -rf tmp_unpack; mkdir -p tmp_unpack
	cd tmp_unpack; gtar -xzf ../doxygen-$(DOX_VER).src.tar.gz; mv doxygen-$(DOX_VER) doxygen-$(DOX_VER).orig
	cd doxygen-$(DOX_VER);
	LC_ALL=C TZ=UTC0 diff -Naur tmp_unpack/doxygen-$(DOX_VER).orig doxygen-$(DOX_VER)  > doxygen.patch; true
	rm -rf tmp_unpack
	@echo " . . . patch file prepared"

#
# Unpack the tar file with the original distribution
#
unpack:
	@./unpack_script $(DOX_VER) $(GRAPH_VER)

#
# Apply the patch
#
patch: last_patched

last_patched: doxygen.patch
	@cd doxygen-$(DOX_VER); patch -p1 < ../doxygen.patch
	@touch last_patched
	@echo " . . . patch applied";\


#___oOo___

#*************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.27 2011/11/28 16:22:27 javarias Exp $"
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
DOX_VER = 1.7.0
GRAPH_VER = 2.26.0
MODULES=graphviz-$(GRAPH_VER) doxygen-$(DOX_VER)

#
# TARGETS
# -------

all:    unpack patch all_first
	chmod -fR +x doxygen-$(DOX_VER)/addon/doxywizard
	@for member in  $(foreach name, $(MODULES), $(name) ) ; do \
		$(MAKE) $(MAKE_PARS) -C $${member} all ;\
	done;
	@echo " . . . 'all' done"

all_first: patch
	@echo "Configuring . . . "
	@./config_all $(DOX_VER) $(GRAPH_VER) > config_all.log 2>&1 

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
	rm -rf doxygen-$(DOX_VER) graphviz-$(GRAPH_VER) config_all.log last_patched


man   : 
	@echo " . . . man page(s) done"

install :
	@for member in  $(foreach name, $(MODULES), $(name) ) ; do \
	$(MAKE) -C $${member} install ;\
	done;
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
patch: unpack last_patched

last_patched: unpack doxygen.patch
	@cd doxygen-$(DOX_VER); patch -p1 < ../doxygen.patch
	@cd graphviz-$(GRAPH_VER); patch -p1 < ../graphviz.patch
	@touch last_patched
	@echo " . . . patch applied";\


#___oOo___

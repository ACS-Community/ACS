#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.6 2010/08/16 12:34:16 mzampare Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# almamgr  12/12/03  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

SHELL=/bin/bash
PLATFORM := $(shell uname)
XSDDOC_VER = 1.0

#
# Jarfiles and their directories
#
JARFILES = xsddoc
xsddoc_DIRS = net
xsddoc_EXTRAS = \
                net/sf/xframe/xsddoc/xslt/component.xsl\
                net/sf/xframe/xsddoc/xslt/help-doc.xsl\
                net/sf/xframe/xsddoc/xslt/index-all.xsl\
                net/sf/xframe/xsddoc/xslt/index.xsl\
                net/sf/xframe/xsddoc/xslt/model.xsl\
                net/sf/xframe/xsddoc/xslt/overview-all.xsl\
                net/sf/xframe/xsddoc/xslt/overview-namespaces.xsl\
                net/sf/xframe/xsddoc/xslt/overview-namespace.xsl\
                net/sf/xframe/xsddoc/xslt/schema-index.xsl\
                net/sf/xframe/xsddoc/xslt/schema-summary.xsl\
                net/sf/xframe/xsddoc/xslt/util.xsl\
                net/sf/xframe/xsddoc/xslt/xml2html.xsl\
                net/sf/xframe/xsddoc/xslt/xmldoc.xsl\
                net/sf/xframe/xsddoc/css/stylesheet.css
#
# Scripts (public and local)
# ----------------------------
SCRIPTS = xsddoc

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
.NOTPARALLEL: all
all:	unpack patch do_all
	@echo " . . . 'all' done" 

clean : clean_all 
	@rm -rf net LICENSE.txt last_unpacked last_patched
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#
# GCH
# This extracts the needed sources from the original
# tar file and moves them in the right place in the tree.
# We will then use a standard ACS Makefile to build.
# I prefer this to extract everything and use the internal
# ant spect because the original tar file contains also 
# the pre-build jar and many other files that are not needed.
# 
unpack : last_unpacked
.PHONY : unpack

# the net outcome of this is a net directory in the CWD
# and an up to date last_unpacked file
#
last_unpacked: xsddoc-$(XSDDOC_VER).tar.gz
	@echo "== Extracting sources from the tar file"
	@gtar -xzf xsddoc-$(XSDDOC_VER).tar.gz xsddoc-$(XSDDOC_VER)/src/net xsddoc-$(XSDDOC_VER)/LICENSE.txt
	@mv xsddoc-$(XSDDOC_VER)/src/net xsddoc-$(XSDDOC_VER)/LICENSE.txt .
	@touch last_unpacked
	@rm -rf xsddoc-$(XSDDOC_VER)

#
# This target prepares the patch file
# after new patches have been applied/coded.
# It assumes that the new/patched files are in
# in 
#     xsddoc
# and unpacks the unpatched code to makethe diff
#     tmp_unpack/xsddoc.orig
#
# Does not use xsddoc as directory name but adds .orig
# to make clearer reading the patch file.
# Before preparing the patch also cleans up the code with the patches
# Makes a copy of the previous patch file for comparison
# and deleted the unpatched code afterwards.
# 
# I had to put a 'true' because patch returns -1. No idea why.  
#
preparePatch:
	@if [ -e xsddoc.patch ]; then \
           mv xsddoc.patch xsddoc.patch.old ;\
         fi
	@rm -rf tmp_unpack; mkdir -p tmp_unpack
	@cd tmp_unpack; \
           gtar -xzf ../xsddoc-$(XSDDOC_VER).tar.gz xsddoc-$(XSDDOC_VER)/src/net; \
           mv xsddoc-$(XSDDOC_VER)/src/net . ;\
           rm -rf xsddoc-$(XSDDOC_VER)
	@LC_ALL=C TZ=UTC0 diff -Naur tmp_unpack/net net  >xsddoc.patch; true
	@rm -rf tmp_unpack
	@echo " . . . patch file prepared"

#
# Apply the patch
#
patch: last_patched

last_patched: xsddoc.patch
	@cd net; patch -p1 < ../xsddoc.patch
	@touch last_patched
	@echo " . . . patch applied";\

#___oOo___

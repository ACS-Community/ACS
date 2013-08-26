#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.33 2009/06/26 13:47:09 eallaert Exp $" 
#
# Makefile of the document utilites
#
# who       when      what
# --------  --------  ---------------------------------------------------
# pforstma  09/09/94  Created from doc, docArchive and docReview Makfiles
# gfilippi  04/11/94  upgrade according to vltMake 1.3
# gfilippi  12/07/95  docPcode added
# gfilippi  13/07/95  use wishx for docPcode 
# gfilippi  21/03/97  use tcl for docPcode 
# fcarbogn  25/07/97  porting FM4 to FM5 
# mverola   21/07/98  added docManExtract and docModManBuild
# mverola   24/08/98  docUnix2Doc changed from script to tcl-script
# psivera   02/02/01  added docDeroff (for MANPAGES to TXT conversion)
# eallaert 2009-06-26 added docPrepareComments
#---------------------------------------------------------------------------


#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile manpage documentation).
#*******************************************************************************
# REMARKS
#    none
#------------------------------------------------------------------------

#
# MODULE CODE DESCRIPTION:
# ------------------------

#
# C programs (public and local)
# -----------------------------
EXECUTABLES   = docDoManPages docA2MIF docExDb docExPc \
                docDoDbTree docSplitIntoSingleComments docSetCommentNumber

#
# Man Pages extractor 
docDoManPages_OBJECTS = docDoManPages

#
# Filter to convert ASCII into MIF
docA2MIF_OBJECTS = docA2MIF

#
# Filter to extract db info
docExDb_OBJECTS = docExDb

#
# Filter to extract pseudo code
docExPc_OBJECTS = docExPc

#
# Display (DB) directory structure
docDoDbTree_OBJECTS = docDoDbTree

#
# doc review procedure tools
docSplitIntoSingleComments_OBJECTS = docSplitIntoSingleComments
docSetCommentNumber_OBJECTS = docSetCommentNumber


# Scripts (public and local)
# ----------------------------
SCRIPTS  =  docFMupdate docDos2Unix docExDbDir docDoDbInfo \
            docArchive doc docSelectDocument docSelectIssue \
            docSelectPreparation docSelectOption docMakeReport \
            docMergeComments docMoveOldVersions docCopyMif\
            docImported4to5.ex docImporting4to5.ex docModManBuild\
	    docDeroff docPrepareComments

#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     = docPcode docManExtract docUnix2Dos
TCL_SCRIPTS_L   =

#
# Gianluca's format Pseudo code extractor
docPcode_OBJECTS  = docPcode
docPcode_TCLSH    = tcl -f

docManExtract_OBJECTS = docManExtract
docManExtract_TCLSH   = seqSh

docUnix2Dos_OBJECTS   = docUnix2Dos
docUnix2Dos_TCLSH     = seqSh



#
#
# man pages to be done
# --------------------
MANSECTIONS = 7
MAN7 = docDoManPages.c docA2MIF.c docFMupdate docExDb.c docExDbDir \
       docExPc.c docDoDbTree.c docDoDbInfo docUnix2Dos.tcl docDos2Unix \
       doc docArchive docMergeComments docSetCommentNumber.c \
       docMoveOldVersions docCopyMif docManExtract docModManBuild docDeroff
#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
        $(foreach exe, $(EXECUTABLES), $($(exe)_OBJECTS))

#
#>>>>> END OF MODULE CODE DESCRIPTION

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
all:    do_all
	@echo " . . . 'all' done"

clean : clean_all
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all
	@echo " . . . clean_dist done"

man   : do_man
	@echo " . . . man page(s) done"

install : install_all
	-if [ -f $(VLTTOP)/paraCatalog.mif ];\
             then $(RM) $(VLTTOP)/paraCatalog.mif;\
         fi;\
         cp paraCatalog.mif $(VLTTOP)
	@echo " . . . installation done"


#___oOo___

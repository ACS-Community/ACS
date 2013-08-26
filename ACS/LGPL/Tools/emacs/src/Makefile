#******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.30 2011/11/28 16:22:27 javarias Exp $"
#
# Makefile of emacs VLT customizations
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi 2002-02-27 Added change mode to u+w for files just copied in ../bin
# gchiozzi 2002-02-27 Force using emacs. There are problems on RH 7.2 trying to make for xemacs.
# gchiozzi 2002-02-09 Added python mode
# rkarban  11/12/98  install also jde files
# jknudstr  28/03/96  Added occam-mode.el and toolset-compile.el
# gchiozzi  14/03/96  Added support for extra files in Makefile
# gchiozzi  14/03/96  Added support for HTML
# gchiozzi  26/06/95  created
# ssandroc  17/07/95  revised install
# gfilippi  21/08/96  add chmod and use of /vlt/System in install
# gfilippi  21/08/96  EXECUTABLES = etags  temporary removed (problem on HP10)
# gfilippi  24/08/98  Emacs and Emacs.local moved to stdEnv


#******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

#
# user definable C-compilation flags
OPTIMIZE = On
USER_CFLAGS = -O -s -DETAGS
YACC   = yacc
YFLAGS = -v
LEX    = lex
LDLIBS = -ll

#
# additional include and library search paths
#USER_INC = 
#USER_LIB = 

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
#EXECUTABLES     = etags   !temporary removed because it does not work on HP10.10
EXECUTABLES     =
EXECUTABLES_L   = 

#
# <brief description of xxxxx program>
etags_OBJECTS   = etags parser
etags_LIBS      = l

#
# special compilation flags for single c sources
etags_CFLAGS   = -O -s -DETAGS
parser_CFLAGS  = -O -s -DETAGS
lexer_CFLAGS   = -O -s -DETAGS

#
# Includes (.h) files (public only)
# ---------------------------------
INCLUDES        =

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       =
LIBRARIES_L     =

#
# <brief description of lllll library>
lllll_OBJECTS   =

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         =
SCRIPTS_L       =

#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     =
TCL_SCRIPTS_L   =

#
# <brief description of tttttt tcl-script>
tttttt_OBJECTS  =
tttttt_TCLSH    = 
tttttt_LIBS     = 

#
# TCL libraries (public and local)
# ------------------------------
TCL_LIBRARIES   =
TCL_LIBRARIES_L =

#
# <brief description of tttlll library>
tttlll_OBJECTS  = 

#
# UIF panels (public and local)
# ----------------------------
PANELS   =
PANELS_L = 

#
# man pages to be done
# --------------------
MANSECTIONS =
MAN1 =
MAN3 =
MAN5 =
MAN7 =
MAN8 =

#
# local man pages
# ---------------
MANl =

#
# ASCII file to be converted into Framemaker-MIF
# --------------------
ASCII_TO_MIF = 


#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES =
#CSOURCENAMES = \
#	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
#	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

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
MAKEDIRTMP := $(shell searchFile include/acsMakefile)
ifneq ($(MAKEDIRTMP),\#error\#)
   MAKEDIR := $(MAKEDIRTMP)/include
   include $(MAKEDIR)/acsMakefile
endif


../bin/%.elc : %.el
	- @echo "== emacs compile : $(<F)"
	$(AT) emacs -batch -l xml-parse.el -f batch-byte-compile  $(<F); mv $(*F).elc ../bin

../bin/%.el : %.el
	- @echo "== emacs lisp : $(<F)"
	$(AT) cp $(*F).el ../bin

W3=w3-4.0pre.47

JDE=jde-2.1.5
EMACS_JDE = \
		beanshell \
		imenu \
		speedbar \
		jde-compile \
		jde-db \
		jde-gen \
		jde-make \
		jde-run \
		jde-wiz \
		jde \
		rpm \
		sb-gud \
		sb-info \
		sb-w3

EMACS_JDE_CMP =

DOXYMACS = doxymacs-1.3.0/lisp
EMACS_DOXYMACS =   doxymacs
EMACS_DOXYMACS_CMP = xml-parse

EMACS_LISP_CMP = \
             hscroll \
             c++-browse\
             vlt-cmm \
             comment \
             vlt-menu \
             vlt-saveconf \
             vlt-tags \
             hilit-change-log   \
             hilit-tcl	\
             add-log \
	     browse-url \
	     html-helper-mode \
	     tempo \
             occam-mode \
             toolset-compile \
             python-mode \
             tempo \
	     $(EMACS_JDE_CMP) \
	     $(EMACS_DOXYMACS_CMP)

EMACS_LISP = $(EMACS_JDE) $(EMACS_DOXYMACS)


#
# TARGETS
# -------
all:	parser.c do_links do_all do_w3 do_emacs
	$(AT) chmod u+w ../bin/*.elc ../bin/*.el
	@echo " . . . 'all' done" 

do_emacs: $(foreach name, $(EMACS_LISP_CMP), ../bin/$(name).elc) \
           $(foreach name, $(EMACS_LISP), ../bin/$(name).el)

do_w3:
	(cd $(W3); chmod u+x ./configure; ./configure --with-emacs --prefix=$(LIB)/emacs; make $(MAKE_PARS)); cp $(W3)/lisp/*.elc $(W3)/lisp/*.el ../bin

clean : clean_all clean_links
	$(RM) ../*~ *~ \#* *.aux *.cp *.dvi *.fn *.ky *.log *.pg *.toc *.tp *.vr *.o TAGS
	$(RM) parser.c lexer.c y.output
	$(RM) ../bin/*.el ../bin/*.elc *.elc
	$(RM) ../object/parser.o ../object/etags.o
	$(AT) if [ -f $(W3)/Makefile ] ; then cd $(W3); make distclean; fi
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	- $(AT) if [ ! -d $(LIB)/emacs ]     ; then mkdir $(LIB)/emacs     ; fi
	- $(AT) if [ ! -d $(LIB)/emacs/lisp ]; then mkdir $(LIB)/emacs/lisp; fi
	- $(AT) if [ ! -d $(LIB)/emacs/Info ]; then mkdir $(LIB)/emacs/Info; fi
	- $(AT) cp ../bin/*.elc ../bin/*.el $(LIB)/emacs/lisp; chmod u+w $(LIB)/emacs/lisp/*
	- $(AT) cp ../info/[a-z]* $(LIB)/emacs/Info; chmod u+w $(LIB)/emacs/Info/*
	- $(AT) if [ ! -d $(VLTTOP)/System ]; then mkdir $(VLTTOP)/System; fi
	- $(AT) echo "\n\n"
	- $(AT) echo "Delete $(LIB)/emacs/Info if you do not need it!"
	- $(AT) echo "Delete $(VLTTOP)/src/emacs if you do not need it!"
	- $(AT) echo "\nTODO:"
	- $(AT) echo "Add two links in order to use the new emacs setup:"
	- $(AT) echo "ln -s $(VLTTOP)/System/Emacs ~/.emacs"
	- $(AT) echo "ln -s $(VLTTOP)/System/Emacs.local ~/.emacs.local"
	@echo " . . . installation done"

COMMON_SOURCES = \
	$(foreach exe, $(EMACS_JDE_CMP) $(EMACS_JDE), $(exe).el)
COMMON_SOURCES_ORIG = \
	$(foreach exe, $(EMACS_JDE_CMP) $(EMACS_JDE), $(JDE)/$(exe).el)

$(COMMON_SOURCES) : $(COMMON_SOURCES_ORIG)
	- @echo "== Linking source: $@"
	- $(AT) $(RM) $@ ; ln -s $(JDE)/$@ $@

COMMON_SOURCES2 = \
	$(foreach exe, $(EMACS_DOXYMACS_CMP) $(EMACS_DOXYMACS), $(exe).el)
COMMON_SOURCES_ORIG2 = \
	$(foreach exe, $(EMACS_DOXYMACS_CMP) $(EMACS_DOXYMACS), $(DOXYMACS)/$(exe).el)

$(COMMON_SOURCES2) : $(COMMON_SOURCES_ORIG2)
	- @echo "== Linking source: $@"
	- $(AT) $(RM) $@ ; ln -s $(DOXYMACS)/$@ $@

do_links: $(COMMON_SOURCES) $(COMMON_SOURCES2)

clean_links:
	$(AT) $(RM) $(COMMON_SOURCES) $(COMMON_SOURCES2)

# Targets to build etags tool
parser.c : parser.y lexer.c
	$(YACC) $(YFLAGS) parser.y
	mv -f y.tab.c $@

lexer.c : lexer.l
	$(LEX) $(LFLAGS) -t lexer.l >$@

#___oOo___

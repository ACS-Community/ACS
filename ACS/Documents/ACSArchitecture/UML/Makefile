#*******************************************************************************
# E.S.O. - ALMA project
#
# "@(#) $Id: Makefile,v 1.9 2009/04/30 08:18:35 gchiozzi Exp $"
#
#

#
# This Makefile must be used from Linux
# It should be easily possible to get it working from
# Windows +  Cygwin, but this was not tested and it is not
# in the current scope.

# Define where to find the Magic Draw binaries.
# Unfortunately it seems there is no better way to do it.

ifndef MAGIC_DRAW_HOME
  export MAGIC_DRAW_HOME=/home/almamgr/MagicDraw_UML/
  export MAGIC_DRAW_SET_LOCALLY:=true
endif

magic_draw_home:
	@echo "Value of MAGIC_DRAW_HOME is: $(MAGIC_DRAW_HOME)"
	@if [ "X$(MAGIC_DRAW_SET_LOCALLY)" != "X" ]; then \
          echo "   The value of MAGIC_DRAW_HOME was set to the default in the Makefile";\
        fi

DESTDIR=out

ifdef TAG
  CVSFLAGS=-r $(TAG)
endif

PLATFORM := $(shell uname)

magic_draw_wp: magic_draw_home
	@echo "Producing HTML from Magic_Draw UML model!"
	@rm -rf Html; mkdir -p Html
	@./mdExtractHtml $(PWD)/ACS.mdzip "ACS Architecture" $(PWD)/Html
	@echo "... Magic_Draw publishing done"

clean:
	@rm -rf Html *~

all: magic_draw_wp

cvsUpdate:
	cvs -Q update -P $(CVSFLAGS) .






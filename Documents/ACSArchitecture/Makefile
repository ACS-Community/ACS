#*******************************************************************************
# E.S.O. - ALMA project
#
# "@(#) $Id: Makefile,v 1.9 2009/04/24 09:58:05 gchiozzi Exp $"
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
	@echo "Producing HTML from Rational Magic_Draw UML model!"
	@rm -rf Html; mkdir -p Html
	@echo "-----> NOT IMPLEMENTED YET!"
	@echo "... Magic_Draw publishing done"

diagrams: magic_draw_home
	@echo "Extract diagrams from HTML model into document!"
	@cd UML; ./mdExtractImages ACS.mdzip ../Project/Images
	@echo "... diagrams extraction done!"

#
# This should call Open Office and update the links in 
# batch, but still does not work!
#
document_update:
	@echo "Updating Open Office document!"
	@echo "-----> NOT IMPLEMENTED YET!"
	@echo "... Open Office update done"

clean:
	@rm -rf Html *~


##### all: rosewp diagrams word_document
all: magic_draw_home magic_draw_wp diagrams document_update

##### model: only rose model, no word document.
model: magic_draw_home diagrams 

cvsUpdate:
	cvs -Q update -P $(CVSFLAGS) .

$(DESTDIR)/%.pdf : %.odt
	@echo "Updating PDF document from Open Office for " $<
	@echo "-----> NOT IMPLEMENTED YET!"
	@echo "... PDF update done"





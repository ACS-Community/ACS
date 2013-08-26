#*******************************************************************************
# E.S.O. - ALMA project
#
# "@(#) $Id: Makefile,v 1.12 2009/04/30 08:30:28 gchiozzi Exp $"
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
	@rm -rf UML/Html; mkdir -p UML/Html
	@UML/mdExtractHtml $(PWD)/UML/ACS.mdzip "ACS Architecture" $(PWD)/UML/Html
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


##### all: magic draw, diagrams word_document
all: model document_update

##### model: only  magic draw model, no word document.
model: magic_draw_home magic_draw_wp diagrams 

cvsUpdate:
	cvs -Q update -P $(CVSFLAGS) .

$(DESTDIR)/%.pdf : %.odt
	@echo "Updating PDF document from Open Office for " $<
	@echo "-----> NOT IMPLEMENTED YET!"
	@echo "... PDF update done"





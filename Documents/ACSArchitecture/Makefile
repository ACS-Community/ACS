#*******************************************************************************
# E.S.O. - ALMA project
#
# "@(#) $Id: Makefile,v 1.7 2005/06/23 10:06:38 gchiozzi Exp $"
#
#

#
# This Makefile must be used from Windows
# within a Cygwin installation

# Define where to find the Rose binaries.
# Unfortunately it seems there is no better way to do it.
export ROSE_HOME:=/cygdrive/c/Program Files/Rational/Rose

DESTDIR=out

ifdef TAG
  CVSFLAGS=-r $(TAG)
endif

PLATFORM := $(shell uname)

rosewp: 
	@cd UML; $(MAKE) rosewp

diagrams:
	@echo "Extract diagrams from HTML model into document!"
	@cd UML; ./extractDiagrams.sh ../Project/Images
	@echo "... diagrams extraction done!"

#
# This should call Microsoft Word and update the links in 
# batch, but still does not work!
#
word_update:
	@cscript /nologo ./DocUpdate.vbs /nologo ACSArchitecture.doc

word_break:
	@cscript /nologo ./DocBreakLinks.vbs /nologo ACSArchitecture.doc ACSArchitectureNL.doc

clean:
	@rm -rf Html *~

#################################### Windows ###############
ifeq ($(PLATFORM),CYGWIN_NT-5.1)
  PLATFORM := Sun

##### all: rosewp diagrams word_document
all: rosewp diagrams word_update word_break

##### model: only rose model, no word document.
model: rosewp diagrams 

cvsUpdate:
	cvs -Q update -P $(CVSFLAGS) .

$(DESTDIR)/%.pdf : %.doc
	@mkdir -p out
	@cscript /nologo ./Doc2Pdf.vbs /nologo $<
	@mv $(<D)/$(@F) $@

#################################### NON Windows ###############
else

.DEFAULT:
	@echo "This Makefile must be run from Windows Cygwin!"

endif
#################################### end ###############



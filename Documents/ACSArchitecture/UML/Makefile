#*******************************************************************************
# E.S.O. - ALMA project
#
# "@(#) $Id: Makefile,v 1.4 2005/05/17 11:21:43 gchiozzi Exp $"
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
	@echo "Producing HTML from Rational Rose UML model!"
	@rm -rf Html; mkdir -p Html
	@./makeRosewp.py
	@echo "... Rose publishing done"

clean:
	@rm -rf Html *~

#################################### Windows ###############
ifeq ($(PLATFORM),CYGWIN_NT-5.1)
  PLATFORM := Sun

all: rosewp

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



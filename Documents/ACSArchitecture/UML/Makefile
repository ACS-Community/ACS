#*******************************************************************************
# E.S.O. - ALMA project
#
# "@(#) $Id: Makefile,v 1.5 2008/01/16 10:03:43 gchiozzi Exp $"
#
#

#
# This Makefile must be used from Windows
# within a Cygwin installation

# Define where to find the Rose binaries.
# Unfortunately it seems there is no better way to do it.

ifndef ROSE_HOME
  export ROSE_HOME:=/cygdrive/e/Rational/Rose
  export ROSE_SET_LOCALLY:=true
endif

rose_home:
	@echo "Value of ROSE_HOME is: $(ROSE_HOME)"
	@if [ "X$(ROSE_SET_LOCALLY)" != "X" ]; then \
          echo "   The value of ROSE_HOME was set to the default in the Makefile";\
        fi

DESTDIR=out

ifdef TAG
  CVSFLAGS=-r $(TAG)
endif

PLATFORM := $(shell uname)

rosewp: rose_home
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



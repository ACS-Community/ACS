#*******************************************************************************
# E.S.O. - ALMA project
#
# "@(#) $Id: Makefile,v 1.11 2007/11/12 13:37:29 gchiozzi Exp $"
#
#

#
# This Makefile must be used from Windows
# within a Cygwin installation
#

#
# Set this variable on the calling make command line
# to redirect the output in the desired directory
#
DESTDIR=out

ifdef TAG
  CVSFLAGS=-r $(TAG)
endif

PLATFORM := $(shell uname)

WORDDOCS := $(wildcard *.doc) \
            ACSArchitecture/ACSArchitectureNL.doc \
            ACSComponentSimulator/ACSComponentSim.doc \
            Acspy/ACSPython.doc \
            Acstime/ACSTime.doc \
            NotificationChannel/Notification_Channel_Module_Software_Design.doc
PDFDOCS  := $(addprefix $(DESTDIR)/, $(addsuffix .pdf, $(basename $(notdir $(WORDDOCS)))))

vpath %.doc .:Acspy:Acstime:NotificationChannel:ACSArchitecture:ACSComponentSimulator

listDocs: 
	@echo Docs to be converted from MS Word to PDF in directory $(DESTDIR):
	@for member in  $(foreach name, $(WORDDOCS), $(name) ) ; do \
		    echo "   $${member}" ;\
		done

#################################### Windows ###############
ifeq ($(PLATFORM),CYGWIN_NT-5.1)
  PLATFORM := Sun

all: cvsUpdate changeLog acsarchitectureupd docs

docs: $(PDFDOCS)

cvsUpdate:
	cvs update -d -P $(CVSFLAGS) .

acsarchitectureupd:
	@echo "Update ACS Architecture UML Model, HTML and pictures."
	@cd ACSArchitecture; $(MAKE) all

changeLog:
	@echo "Generating changeLog"
	@cp changeLogHeader.txt $(DESTDIR)/changeLog.txt
	@echo >> $(DESTDIR)/changeLog.txt
	@echo "Generated:" `date` >> $(DESTDIR)/changeLog.txt
	@echo >> $(DESTDIR)/changeLog.txt
	@rcs2log . >> $(DESTDIR)/changeLog.txt

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



# ******************************************************************************
# E.S.O. - ALMA project
#
# "@(#) $Id: Makefile,v 1.13 2009/04/30 10:42:36 gchiozzi Exp $"
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
            ACSComponentSimulator/ACSComponentSim.doc \
            Acspy/ACSPython.doc \
            Acstime/ACSTime.doc \
            NotificationChannel/Notification_Channel_Module_Software_Design.doc
OODOCS   := $(wildcard *.odt) \
            ACSArchitecture/ACSArchitecture.odt
ALLDOCS  := $(WORDDOCS) $(OODOCS)
PDFDOCS  := $(addprefix $(DESTDIR)/, $(addsuffix .pdf, $(basename $(notdir $(ALLDOCS)))))

vpath %.doc .:Acspy:Acstime:NotificationChannel:ACSArchitecture:ACSComponentSimulator
vpath %.odt .:Acspy:Acstime:NotificationChannel:ACSArchitecture:ACSComponentSimulator

listDocs: 
	@echo Docs to be converted from MS Word or Open Office to PDF in directory $(DESTDIR):
	@for member in  $(foreach name, $(ALLDOCS), $(name) ) ; do \
		    echo "   $${member}" ;\
		done

all: loginfo svnUpdate changeLog acsarchitectureupd docs

docs: $(PDFDOCS)

loginfo:
	@echo Generating ACS Documentation on Windows
	@echo I am in: $(PWD)
	@echo Target is: $(DESTDIR)

svnUpdate:
	svn update

acsarchitectureupd:
	@echo "Update ACS Architecture UML Model, HTML and pictures."
	@cd ACSArchitecture; $(MAKE) all

changeLog:
	@echo "Generating changeLog"
	@cp changeLogHeader.txt $(DESTDIR)/changeLog.txt
	@echo >> $(DESTDIR)/changeLog.txt
	@echo "Generated:" `date` >> $(DESTDIR)/changeLog.txt
	@echo >> $(DESTDIR)/changeLog.txt
	@svn log -v >> $(DESTDIR)/changeLog.txt
	@echo "ChangeLog generated."

$(DESTDIR)/%.pdf : %.doc
	@mkdir -p out
	@./OpenOfficeDocumentConverter.sh $< $@

$(DESTDIR)/%.pdf : %.odt
	@mkdir -p out
	@./OpenOfficeDocumentConverter.sh $< $@


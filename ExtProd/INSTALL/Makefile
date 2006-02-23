#*******************************************************************************
# E.S.O. - ALMA project
#
# "@(#) $Id: Makefile,v 1.10 2005/10/18 12:05:22 gchiozzi Exp $"
#
# Makefile of ........
#
aips:	
	@chmod +x *
	@./buildAIPS++
	@echo " . . . 'aips++' done" 

#
# This is used to force building also VxWroks parts on Linux.
#
all-vw:	
	@chmod +x *
	@./buildTools -vw
	@echo " . . . 'all including VxWorks(except aips++)' done" 

all:	
	@chmod +x *
	@./buildTools
	@echo " . . . 'all (except aips++)' done" 

clean : 
	rm -rf ../PRODUCTS/Python-2.4.1
	rm -rf ../PRODUCTS/gcc-SUN
	rm -rf ../PRODUCTS/gnu
	rm -rf ../PRODUCTS/java
	rm -rf ../PRODUCTS/tcltk
	rm -rf ../PROPRIETARY/CCSLite
	rm -rf ../PROPRIETARY/vw
	rm -rf *.log *~
	@echo " . . . clean done"

clean_dist : clean 
	rm -rf *.log *~
	@echo " . . . clean dist done"


#___oOo___

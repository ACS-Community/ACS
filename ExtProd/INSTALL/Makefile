#*******************************************************************************
# E.S.O. - ALMA project
#
# "@(#) $Id: Makefile,v 1.24 2013/02/15 09:11:52 acaproni Exp $"
#
# Makefile of ........
#
#
# This is used to force building also VxWroks parts on Linux.
#
all-vw:	
	@chmod +x *
	@./buildTools -vw
	@echo " . . . 'all including VxWorks' done" 

all:	
	@chmod +x *
	@./buildTools
	@echo " . . . 'all' done" 

clean : 
	rm -rf ../PRODUCTS/Python-2.6.5
	rm -rf ../PRODUCTS/gnuplot-py-1.8
	rm -rf ../PRODUCTS/pexpect-2.3
	rm -rf ../PRODUCTS/lxml-2.2.6
	rm -rf ../PRODUCTS/nose-0.11.3
	rm -rf ../PRODUCTS/gcc-SUN
	rm -rf ../PRODUCTS/gnu
	rm -rf ../PRODUCTS/java
	rm -rf ../PRODUCTS/tcltk
	rm -rf ../PRODUCTS/mico
	rm -rf ../NO-LGPL/CCSLite
	rm -rf ../NO-LGPL/vw
	rm -rf *.log *~
	@echo " . . . clean done"

clean_dist : clean 
	rm -rf *.log *~
	@echo " . . . clean dist done"


#___oOo___

#*******************************************************************************
# E.S.O. - ALMA project
#
# "@(#) $Id: Makefile,v 1.11 2007/07/10 22:22:26 agrimstrup Exp $"
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
	rm -rf ../PRODUCTS/pysnmp-2.0.9
	rm -rf ../PRODUCTS/python-ldap-2.0.1
	rm -rf ../PRODUCTS/Numeric-23.1
	rm -rf ../PRODUCTS/numarray-1.3.3
	rm -rf ../PRODUCTS/numpy-1.0.1
	rm -rf ../PRODUCTS/gnuplot-py-1.7
	rm -rf ../PRODUCTS/matplotlib-0.73.1
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

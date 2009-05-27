#*******************************************************************************
# E.S.O. - ALMA project
#
# "@(#) $Id: Makefile,v 1.16 2009/05/27 19:03:07 agrimstrup Exp $"
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
	rm -rf ../PRODUCTS/Python-2.5.2
	rm -rf ../PRODUCTS/pysnmp-2.0.9
	rm -rf ../PRODUCTS/python-ldap-2.3.5
	rm -rf ../PRODUCTS/Numeric-24.2
#	rm -rf ../PRODUCTS/numarray-1.5.2
	rm -rf ../PRODUCTS/numpy-1.1.1
	rm -rf ../PRODUCTS/scipy-0.6.0
	rm -rf ../PRODUCTS/gnuplot-py-1.8
	rm -rf ../PRODUCTS/matplotlib-0.91.4
	rm -rf ../PRODUCTS/pexpect-2.3
	rm -rf ../PRODUCTS/pychecker-0.8.17
	rm -rf ../PRODUCTS/setuptools-0.6c8
	rm -rf ../PRODUCTS/lxml-2.1.5
	rm -rf ../PRODUCTS/generateDS-1.15d
	rm -rf ../PRODUCTS/pythoscope-0.3.2
	rm -rf ../PRODUCTS/snakefood-1.3.1
	rm -rf ../PRODUCTS/nose-0.10.4
	rm -rf ../PRODUCTS/figleaf-latest
	rm -rf ../PRODUCTS/logilab-common-0.39.0
	rm -rf ../PRODUCTS/logilab-astng-0.19.0
	rm -rf ../PRODUCTS/pylint-0.18.0
#	rm -rf ../PRODUCTS/ipython-0.8.1
#	rm -rf ../PRODUCTS/ipython1-saw
#	rm -rf ../PRODUCTS/Twisted-2.5.0
	rm -rf ../PRODUCTS/coverage-2.80
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

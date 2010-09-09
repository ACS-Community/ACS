#*******************************************************************************
# E.S.O. - ALMA project
#
# "@(#) $Id: Makefile,v 1.22 2010/09/09 00:53:02 agrimstrup Exp $"
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
	rm -rf ../PRODUCTS/pysnmp-4.1.13a
	rm -rf ../PRODUCTS/python-ldap-2.3.11
	rm -rf ../PRODUCTS/numpy-1.4.1
	rm -rf ../PRODUCTS/scipy-0.7.2
	rm -rf ../PRODUCTS/gnuplot-py-1.8
	rm -rf ../PRODUCTS/matplotlib-0.99.1.1
	rm -rf ../PRODUCTS/pexpect-2.3
	rm -rf ../PRODUCTS/pychecker-0.8.17
	rm -rf ../PRODUCTS/distribute-0.6.10
	rm -rf ../PRODUCTS/lxml-2.2.6
	rm -rf ../PRODUCTS/pythoscope-0.4.3
	rm -rf ../PRODUCTS/nose-0.11.3
	rm -rf ../PRODUCTS/logilab-common-0.50.1
	rm -rf ../PRODUCTS/logilab-astng-0.20.0
	rm -rf ../PRODUCTS/pylint-0.20.0
	rm -rf ../PRODUCTS/Pygments-1.3.1
	rm -rf ../PRODUCTS/Jinja2-2.4.1
	rm -rf ../PRODUCTS/docutils-0.6
	rm -rf ../PRODUCTS/Sphinx-0.6.5
	rm -rf ../PRODUCTS/zope.interface-3.6.1
	rm -rf ../PRODUCTS/Twisted-10.0.0
	rm -rf ../PRODUCTS/pyOpenSSL-0.10
	rm -rf ../PRODUCTS/foolscap-0.5.1
	rm -rf ../PRODUCTS/ipython-0.10
	rm -rf ../PRODUCTS/coverage-3.3.1
	rm -rf ../PRODUCTS/gcc-SUN
	rm -rf ../PRODUCTS/gnu
	rm -rf ../PRODUCTS/java
	rm -rf ../PRODUCTS/tcltk
	rm -rf ../PRODUCTS/mico
	rm -rf ../PROPRIETARY/CCSLite
	rm -rf ../PROPRIETARY/vw
	rm -rf ../PRODUCTS/snakefood-1.4
	rm -rf *.log *~
	@echo " . . . clean done"

clean_dist : clean 
	rm -rf *.log *~
	@echo " . . . clean dist done"


#___oOo___

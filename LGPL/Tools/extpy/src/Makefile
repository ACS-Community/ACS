#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.47 2013/02/14 17:12:20 acaproni Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# almamgr 2003-08-27 Removed test and dbl targets
# almamgr 2003-08-27 Replaced tar with tar for sun
# dave 2003-08-25 GENERATEDS is no longer part of the "all" target
# dave 2003-08-14 added PyXML and generateDS for Python XML entity classes
# david  07/07/03  created
#

#
# Python stuff (public and local)
# ----------------------------
PY_SCRIPTS         = pythfilter
PY_SCRIPTS_L       =

PY_MODULES         = acs_python 
PY_MODULES_L       =

PY_PACKAGES        = Pmw
PY_PACKAGES_L      =
pppppp_MODULES	   =

PY_EXT_DOC_L	   = pexpect-doc
PY_DISUTIL	   = pexpect-0.999 PyXML-0.8.4 pychecker-0.8.14 Pmw Numeric-24.2 numarray-1.3.3 python-ldap-2.0.1 gnuplot-py-1.7 pysnmp-2.0.9


PEXPECT: pexpect-0.999.tgz
	@echo "== Building PEXPECT"
	@tar -zxvf pexpect-0.999.tgz;
	@cd pexpect-0.999; python setup.py install --home=$(INSTALL_ROOT) --install-purelib=$(INSTALL_ROOT)/lib/python/site-packages --install-platlib=$(INSTALL_ROOT)/lib/python/site-packages --install-data=$(INSTALL_ROOT)/lib/python/site-packages ;

PYXML: PyXML-0.8.4.tar.gz
	@echo "== Building PYXML"
	@tar -zxvf PyXML-0.8.4.tar.gz; 
	@cd PyXML-0.8.4; python setup.py build --with-libexpat=$(ACSROOT)/lib; python setup.py install --home=$(INSTALL_ROOT) --install-purelib=$(INSTALL_ROOT)/lib/python/site-packages --install-platlib=$(INSTALL_ROOT)/lib/python/site-packages;

LXML: lxml-2.2.6.tar.gz
	@echo "== Building LXML"
	@tar -zxvf lxml-2.2.6.tar.gz; 
	@cd lxml-2.2.6; python setup.py build; python setup.py install --home=$(INSTALL_ROOT) --install-purelib=$(INSTALL_ROOT)/lib/python/site-packages --install-platlib=$(INSTALL_ROOT)/lib/python/site-packages;

PYXB: PyXB-full-1.1.2.tar.gz
	@echo "== Building PyXB"
	@tar -zxvf PyXB-full-1.1.2.tar.gz;
	@cd PyXB-1.1.2; patch -p1 < ../pyxb-datatypes-repr.patch;$(PYTHON_ROOT)/bin/python setup.py build; $(PYTHON_ROOT)/bin/python setup.py install --home=$(INSTALL_ROOT) --install-purelib=$(INSTALL_ROOT)/lib/python/site-packages --install-platlib=$(INSTALL_ROOT)/lib/python/site-packages;

PMW: Pmw.1.2.tar.gz
	@echo "== Building Python Mega Widgets"
	@tar -zxvf Pmw.1.2.tar.gz;

#
# We need a patch for pychecker since the setup.py script
# is script in such a way that it does not allow to properly install the
# package anywhere else than in the Python standard installation tree.
#
PYCHECKER: pychecker-0.8.14.tar.gz
	@echo "== Patching and building PyChecker"
	@tar -zxvf pychecker-0.8.14.tar.gz;
	@patch pychecker-0.8.14/setup.py pychecker-0.8.14.setup.py.patch
	@cd pychecker-0.8.14; python setup.py install --home=$(INSTALL_ROOT) --install-purelib=$(INSTALL_ROOT)/lib/python/site-packages --install-platlib=$(INSTALL_ROOT)/lib/python/site-packages;

NUMERIC: Numeric-24.2.tar.gz
	@echo "== Building Numeric"
	@tar -zxvf Numeric-24.2.tar.gz;
	@cd Numeric-24.2; patch -p1 < ../Numeric-gettimeofday.patch; python setup.py install --home=$(INSTALL_ROOT) --install-purelib=$(INSTALL_ROOT)/lib/python/site-packages --install-platlib=$(INSTALL_ROOT)/lib/python/site-packages; 

NUMARRAY: numarray-1.3.3.tar.gz
	@echo "== Building numarray"
	@tar -zxvf numarray-1.3.3.tar.gz;
	@cd numarray-1.3.3; python setup.py install --gencode; 

LDAP: python-ldap-2.0.1.tar.gz
	@echo "== Building Python LDAP"
	@tar -zxvf python-ldap-2.0.1.tar.gz;
	@cd python-ldap-2.0.1; cat setup.cfg | sed 's/local\/openldap-REL_ENG_2_1\///' >> setup.cfg.tmp
	@mv python-ldap-2.0.1/setup.cfg.tmp python-ldap-2.0.1/setup.cfg 
	@cd python-ldap-2.0.1; python setup.py install;

GNUPLOT: gnuplot-py-1.7.tar.gz
	@echo "== Building GnuPlot Py"
	@tar -zxvf gnuplot-py-1.7.tar.gz;
	@cd gnuplot-py-1.7; python setup.py install;

SNMP: pysnmp-2.0.9.tar.gz
	@echo "== Building PySNMP"
	@tar -zxvf pysnmp-2.0.9.tar.gz;
	@cd pysnmp-2.0.9; python setup.py install;

SUDS: suds-0.4.tar.gz
	@echo "== Building suds"
	@tar -zxvf suds-0.4.tar.gz;
	@cd suds-0.4; python setup.py install --home=$(INSTALL_ROOT) --install-purelib=$(INSTALL_ROOT)/lib/python/site-packages --install-platlib=$(INSTALL_ROOT)/lib/python/site-packages;

#
# list all python tarballs
# ------------------------------
PY_DOC :
	$(foreach file, $(PY_EXT_DOC_L), - $(AT) cd ../doc; if [ -e $(file).tgz ]; then echo "== Extracting external documentation: $(file).tgz"; tar -zxvf $(file).tgz; fi )


#PY_DISUTIL_BUILD : PEXPECT PYXML PYCHECKER NUMERIC NUMARRAY LDAP GNUPLOT SNMP PYXML
PY_DISUTIL_BUILD : PYXB PYXML LXML NUMERIC SUDS
	echo "== Building/Installing external Python modules now..."

#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

#
#>>>>> END OF standard rules

#
# INCLUDE STANDARDS
# -----------------
MAKEDIRTMP := $(shell searchFile include/acsMakefile)
ifneq ($(MAKEDIRTMP),\#error\#)
   MAKEDIR := $(MAKEDIRTMP)/include
   include $(MAKEDIR)/acsMakefile
endif


#
# TARGETS
# -------
all:	PMW do_all PY_DISUTIL_BUILD
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) *~ ../*~
	$(RM) ../doc/doc ../doc/api ../doc/idl
	$(foreach file, $(PY_DISUTIL), $(RM) $(file))
	@rm -rf numarray-1.3.3 pexpect-0.999 Pmw pychecker-0.8.14 PyXML-0.8.4 PyXB-1.1.2 lxml-2.2.6 suds-0.4
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man PY_DOC
	@echo " . . . man page(s) done"

#install : install_all MATPLOTLIB_CONFIG
install : install_all 
	@echo " . . . installation done"

#___oOo___

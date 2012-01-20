#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id: Makefile,v 1.7 2012/01/20 23:18:16 tstaig Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# rtobar  02/12/09  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------


#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = hibernateCdbJDal
SCRIPTS_L       =


#
# Python stuff (public and local)
# ----------------------------
PY_SCRIPTS         =
PY_SCRIPTS_L       =

PY_MODULES         =
PY_MODULES_L       =

PY_PACKAGES        =
PY_PACKAGES_L      =
pppppp_MODULES	   =



#
# Configuration Database Files
# ----------------------------
CDB_SCHEMAS = ControlDevice



#
# Jarfiles and their directories
#
JARFILES=cdb_rdb
cdb_rdb_DIRS=com alma/TMCDB alma/acs/tmcdb/logic
cdb_rdb_EXTRAS=alma/TMCDB/maci/hibernate-mappings-maci.hbm.xml \
               alma/TMCDB/baci/hibernate-mappings-baci.hbm.xml \
               acsOnly-cdb_rdb-hibernate.cfg.xml

cdb_rdb_JLIBS=TMCDBswconfigStrategy cdbrdb-pojos
#
# java sources in Jarfile on/off
DEBUG=on


#
# other files to be installed
#----------------------------
POJOS_JAR = cdbrdb-pojos.jar
INSTALL_FILES = ../lib/$(POJOS_JAR) ../lib/TMCDBswconfigStrategy.jar


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
all:	.done_generating_sql ../lib/TMCDBswconfigStrategy.jar .done_generating_classes ../lib/$(POJOS_JAR) do_all
	@echo " . . . 'all' done" 

clean : clean_all 
	@echo "== Deleting generated code"
	@rm -rf ../config/TMCDB_swconfigcore
	@rm -rf ../config/TMCDB_swconfigext
	@rm -rf tmcdb
	@rm -rf gen
	@rm -f .done_generating_sql
	@rm -f .done_generating_classes
	@rm -f CreateHsqldbTables.sql
	@rm -f alma/acs/tmcdb/translator/Column2Attribute_*
	@rm -f alma/acs/tmcdb/translator/Table2Class_*
	@rm -f alma/acs/tmcdb/translator/TableInheritance_*
	@rm -f alma/acs/tmcdb/translator/*.class
	@rm -f ../lib/$(POJOS_JAR)
	@rm -f ../lib/TMCDBswconfigStrategy.jar
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

DDLDATA=$(ACSDATA)/config/DDL
install : install_all
	@mkdir -p $(DDLDATA)/generic
	@mkdir -p $(DDLDATA)/oracle/TMCDB_swconfigcore
	@mkdir -p $(DDLDATA)/oracle/TMCDB_swconfigext
	@mkdir -p $(DDLDATA)/hsqldb/TMCDB_swconfigcore
	@mkdir -p $(DDLDATA)/hsqldb/TMCDB_swconfigext
	@echo "== Copying generic .ddl files to $(DDLDATA)/generic"
	@cp generic/TMCDB_swconfigcore.ddl $(DDLDATA)/generic
	@cp generic/TMCDB_swconfigext.ddl $(DDLDATA)/generic
	@echo "== Copying generated Oracle .sql files to $(DDLDATA)/oracle"
	@cp ../config/TMCDB_swconfigcore/oracle/* $(DDLDATA)/oracle/TMCDB_swconfigcore
	@cp ../config/TMCDB_swconfigext/oracle/* $(DDLDATA)/oracle/TMCDB_swconfigext
	@echo "== Copying generated HSQLDB .sql files to $(DDLDATA)/hsqldb"
	@cp ../config/TMCDB_swconfigcore/hsqldb/* $(DDLDATA)/hsqldb/TMCDB_swconfigcore
	@cp ../config/TMCDB_swconfigext/hsqldb/* $(DDLDATA)/hsqldb/TMCDB_swconfigext
	@echo " . . . installation done"

.done_generating_sql : generic/TMCDB_swconfigcore.ddl generic/TMCDB_swconfigext.ddl
	@echo "=="
	@echo "== Generating SQL code"
	@echo "=="
	@generateTmcdbSchemas generic/TMCDB_swconfigcore.ddl ../config
	@generateTmcdbSchemas generic/TMCDB_swconfigext.ddl ../config
	@echo "=="
	@echo "== Generating SQL/Java translation code"
	@echo "=="
	@generateTmcdbHibernateStrategy generic/TMCDB_swconfigcore.ddl .
	@generateTmcdbHibernateStrategy generic/TMCDB_swconfigext.ddl .
	@touch .done_generating_sql

.done_generating_classes :../lib/TMCDBswconfigStrategy.jar
	@echo "=="
	@echo "== Generating Java domain classes"
	@echo "=="
	@-rm -rf tmcdb
	@-mkdir tmcdb
	@rm -f CreateHsqldbTables.sql
	@cat ../config/TMCDB_swconfigcore/hsqldb/CreateHsqldbTables.sql ../config/TMCDB_swconfigext/hsqldb/CreateHsqldbTables.sql > CreateHsqldbTables.sql
	@acsStartJava org.hsqldb.cmdline.SqlTool --rcFile sqltool.rc tmcdb CreateHsqldbTables.sql
	@CLASSPATH="$(shell vltMakeJavaClasspath)" ant generate
	@touch .done_generating_classes

../lib/$(POJOS_JAR): .done_generating_classes
	@echo "=="
	@echo "== Compiling generated domain classes"
	@cd gen; CLASSPATH="$(shell vltMakeJavaClasspath)" javac alma/acs/tmcdb/*.java; jar cf ../../lib/$(POJOS_JAR) alma/acs/tmcdb/*.class; cd ..; mv gen src; jar uf ../lib/$(POJOS_JAR) src/alma/acs/tmcdb/*.java; mv src gen/

../lib/TMCDBswconfigStrategy.jar :.done_generating_sql 
	@echo "== Compiling TMCDBswconfigStrategy.jar "
	@CLASSPATH="$(shell vltMakeJavaClasspath)" javac alma/acs/tmcdb/translator/*.java; jar cf ../lib/TMCDBswconfigStrategy.jar alma/acs/tmcdb/translator/*.class; cd ..; jar uf lib/TMCDBswconfigStrategy.jar src/alma/acs/tmcdb/translator/*.java;
	@rm -f alma/acs/tmcdb/translator/*.class


#___oOo___

# $Id: acsMakefileCore.mk,v 1.6 2011/03/31 21:22:12 jagonzal Exp $
#
##################################################################
## DEFINITIONS
##################################################################
IDL_EXTENSIONS=C.h C.cpp S.cpp C.inl S.h S.inl
CASTOR:= alma.tools.entitybuilder.CastorBuilder
OMNI_IDL=omniidl
TAO_IDL = $(TAO_ROOT)/TAO_IDL/tao_idl
#TAO_IDLFLAGS+=-ciC.i -siS.i -stS_T.i
#TAO_IDLFLAGS+=-ci -si -st
### OMNI_IDL = $(OMNI_ROOT)/bin/$(FARCH)/omniidl
TAO_LIBRARIES:=ACE \
               TAO \
               TAO_DsLogAdmin \
               TAO_CosNaming \
               TAO_IORTable \
               TAO_PortableServer \
               TAO_Svc_Utils \
               TAO_CosTrading \
               TAO_CosNotification \
               TAO_DynamicAny \
               TAO_IFR_Client \
               TAO_CosProperty

OMNI_IDL = omniidl
JAVA_IDL = $(JACORB_HOME)/bin/idl
JACORB_MK_IDL_PATH = -I$(JACORB_HOME)/idl/jacorb -I$(JACORB_HOME)/idl/omg
XMLIDL = xmlIdlCompiler
AlmaIDLMainClass=alma.tools.idlgen.XmlIdlCompiler
ABEANSCOMP=abeans.models.acs.baci.generator.IDLtoAbeansCompiler
#
#  RTAI
ifneq ($(strip $(RTAI_HOME)),)
RTAI_CFLAGS = -D__KERNEL__ -DMODULE -O2 -Wall -Wstrict-prototypes -Wno-trigraphs  -fomit-frame-pointer -fno-strict-aliasing -fno-common -pipe  -march=i686 -falign-functions=4 -I$(LINUX_HOME)/include/linux -I$(LINUX_HOME)/include/asm-i386/mach-default $(USER_RTAI_CFLAGS) 
RTAI_CONFIG := $(RTAI_HOME)/bin/rtai-config
KDIR := $(shell $(RTAI_CONFIG) --linux-dir)
CCRTAI:=$(shell $(RTAI_CONFIG) --cc)
USR_INC = -I$(RTAI_HOME)/include  $(patsubst -I..%,-I$(PWD)/..%,$(I_PATH))
EXTRA_CFLAGS = $(shell $(RTAI_CONFIG) --module-cflags) -Werror-implicit-function-declaration  $(patsubst ..%,$(PWD)/..%,$(USR_INC)) $(USER_RTAI_CFLAGS) -DRTAI_HOME
endif
#
ifeq ($(strip $(DEBUG)),on)
  javaCompilerOptions=-g
else
  javaCompilerOptions=
endif

ifneq ($(strip $(COMPONENT_HELPERS)),)
 CompHelpDoGenerateDef=-Dalma.acs.tools.comphelpergen.doGenerate=true
ifneq ($(strip $(XML_IDL)),)
else
 XML_IDL=NOTUSED
endif
else
 CompHelpDoGenerateDef=-Dalma.acs.tools.comphelpergen.doGenerate=false
endif
CompHelpOutRootDirDef=-Dalma.acs.tools.comphelpergen.outRootDir=.

XMLIDL=java $(CompHelpDoGenerateDef) $(CompHelpOutRootDirDef) -DACS.idl2jbind=$(XML_IDL) $(AlmaIDLMainClass) -d $$(TMPSRC) -notie $(verboseDef) 

ACS_CO=ACScomponents
JAVAC ?=javac -J-Xmx1g
ifeq ($(strip $(PYTHON_VERS)),)
PYTHON_VERS:=$(shell python -V 2>&1 | awk '{print $$2}' | awk -F. '{print $$1 "." $$2}')
endif
PYTHON_DOCS=../doc/api/html/python
PYTHONPATHDOC=${PYTHONPATH}:../../../../src

#################################################################
## SCRIPTS
#################################################################
#
# if the list of scripts is not empty, include script-dependencies files. 
SCRIPT_LIST = $(SCRIPTS) $(SCRIPTS_L) 

ifneq "$(strip $(SCRIPT_LIST))" "" 
$(eval $(call top-level,script,$(SCRIPT_LIST),$(SCRIPTS)))
$(foreach scr,$(SCRIPT_LIST),$(eval $(call acsMakeScriptDependencies,$(scr))))
endif

#################################################################
## LOGTS
#################################################################
ifneq "$(strip $(ACSLOGTSDEF))" "" 
tl-java-prereq +=do_logtss 
$(eval $(call top-level,logts,$(ACSLOGTSDEF),$(ACSLOGTSDEF)))

$(foreach logts,$(ACSLOGTSDEF), \
	$(eval $(call acsMakeLibraryDependencies,,$(logts)LTS,$(logts),,,)) )

$(foreach logts,$(ACSLOGTSDEF), \
	$(eval $(call acsMakeLogTSDependencies,$(logts)) ) )

endif

#################################################################
## XSD
#################################################################

#vpath %.xsdl $(MK_IDL_PATH_LIST)
vpath %.xsd ../idl

#

ifndef MAKE_VXWORKS
ifdef XSDBIND
$(eval $(call top-level,xsdbind,$(XSDBIND),$(XSDBIND)))
tl-java-prereq += do_xsdbinds
tl-idl-prereq += do_xsdbinds
endif
endif

ifneq "$(strip $(XSDBIND))" "" 
$(foreach xsd,$(XSDBIND),$(eval $(call XSDPrereq,$(xsd))))
$(foreach xsd,$(XSDBIND),$(eval $(call acsMakeXSDDependencies,$(xsd),$(XSDBIND_INCLUDE))))
endif
#################################################################
## XMLERR
#################################################################



ifdef ACSERRDEF
$(eval $(call top-level,xmlerr,$(ACSERRDEF),$(ACSERRDEF)))
tl-java-prereq += do_xmlerrs
tl-idl-prereq += do_xmlerrs
endif

ifdef MAKE_VXWORKS
$(foreach xml,$(ACSERRDEF),\
	$(eval $(call acsMakeExecutableDependencies,/vw,$(xml),$(xml)S $(xml)C,,,)) \
)
else
$(foreach xml,$(ACSERRDEF),\
	$(eval $(call acsMakeLibraryDependencies,,$(xml),$(xml) $(xml)S $(xml)C,,,acserr )) \
)
endif


$(foreach xml,$(ACSERRDEF),$(eval $(call XMLPrereq,$(xml))))
$(foreach xml,$(ACSERRDEF),$(eval $(call acsMakeXMLErrDependencies,$(xml))))
#
ifndef MAKE_VXWORKS
$(foreach xml,$(ACSERRDEF),$(eval $(call acsMakeLibraryDependencies,,$(xml)Stubs,$(xml)S $(xml)C,,,$(call unique,$($(xml)Stubs_LIBS) $(TAO_LIBRARIES)) acserr acserrStubs)))
else
$(foreach xml,$(ACSERRDEF),$(eval $(call acsMakeExecutableDependencies,/vw,$(xml)Stubs,$(xml)S $(xml)C, , , )))
endif

$(foreach xml,$(ACSERRDEF),$(eval $(call IDLPrereq,$(xml))) )
$(foreach xml,$(ACSERRDEF),$(eval $(call acsMakeIDLDependencies,$(xml),idl)))



#################################################################
## IDL
#################################################################
# set search path for idl files

vpath %.idl $(MK_IDL_PATH_LIST) $(INTROOT)/idl $(ACSROOT)/idl $(subst -I, ,$(TAO_MK_IDL_PATH))
vpath %.pidl $(MK_IDL_PATH_LIST) $(INTROOT)/idl $(ACSROOT)/idl $(subst -I, ,$(TAO_MK_IDL_PATH))
vpath %.midl $(MK_IDL_PATH_LIST) $(INTROOT)/idl $(ACSROOT)/idl $(subst -I, ,$(TAO_MK_IDL_PATH))


IDL_LIST=$(IDL_FILES) $(IDL_FILES_L)

#

ifeq ($(call mustBuild,C++),true)
ifdef MAKE_VXWORKS
$(foreach idl,$(IDL_LIST),$(eval $(call acsMakeExecutableDependencies,/vw,$(idl)Stubs,$(idl)S $(idl)C, , , )))
else
$(foreach idl,$(IDL_LIST),$(eval $(call acsMakeLibraryDependencies,,$(idl)Stubs,$(idl)S $(idl)C,,,$(call unique,$($(idl)Stubs_LIBS) $(TAO_LIBRARIES)))))
endif
endif

$(foreach idl,$(IDL_LIST),$(eval $(call IDLPrereq,$(idl))))
$(foreach idl,$(IDL_LIST),$(eval $(call acsMakeIDLDependencies,$(idl),idl)))

ifneq ($(strip $(IDL_LIST)),)
$(eval $(call top-level,idl,$(IDL_LIST),$(IDL_FILES)))
endif


#################################################################
## JAVA
#################################################################


JARFILES_LIST = $(JARFILES) $(JARFILES_L)
C_JARFILES_LIST = $(COMPONENTS_JARFILES) $(COMPONENTS_JARFILES_L)

ifndef MAKE_VXWORKS
ifeq ($(call mustBuild,Java),true)


ifneq ($(strip $(JARFILES_LIST) $(C_JARFILES_LIST)),)
$(eval $(call top-level,java,\
	$(JARFILES_LIST) $(C_JARFILES_LIST),$(JARFILES) $(COMPONENTS_JARFILES)))
endif 
endif # must build Java
endif # no VXWORKS


ifneq ($(strip $(JARFILES_LIST)),)
$(foreach jar,$(JARFILES_LIST),$(eval $(call JARPrereq,$(jar))))
$(foreach jar,$(JARFILES_LIST),$(eval $(call acsMakeJavaDependencies,$(jar),$($(jar)_DIRS),$($(jar)_EXTRAS),off,$(USER_JFLAGS),$($(jar)_JFLAGS),$($(jar)_ENDORSED),$(DEBUG),$($(jar)_JLIBS))))

endif

ifneq ($(strip $(C_JARFILES_LIST)),)
$(foreach jar,$(C_JARFILES_LIST),$(eval $(call JARPrereq,$(jar))))
$(foreach jar,$(C_JARFILES_LIST),$(eval $(call acsMakeJavaDependencies,$(jar),$($(jar)_DIRS),$($(jar)_EXTRAS),on,$(USER_JFLAGS),$($(jar)_JFLAGS),$($(jar)_ENDORSED),$(DEBUG),$($(jar)_JLIBS))))
endif

#.NOTPARALLEL: do_javas

javadoc: 
	$(AT)echo "......Javadoc:"
	$(AT)mkdir -p ../doc/api/html
	$(AT)for member in $(foreach jarfile, $(JARFILES_LIST), $($(jarfile)_DIRS)) ;\
		do \
		javadoc $(JavadocOptions) -classpath `vltMakeJavaClasspath` -d ../doc/api/html `find $${member} -type f -name \*.java` > /dev/null 2>&1 ;\
		done



#################################################################
## DDS
#################################################################
ifdef DDS_FILES
$(eval $(call top-level,dds,$(DDS_FILES),$(DDS_FILES)))
endif

$(foreach dds,$(DDS_FILES),\
 $(if $(dds), \
   $(eval $(call acsMakeLibraryDependencies,,$(dds)Stubs,$(dds)S $(dds)C,,,$($(dds)Stubs_LIBS))) \
   $(if $(wildcard ../idl/$(dds).midl),\
     $(eval $(dds)_typesList = $(call typesFromDDS,../idl/$(dds).midl)),\
       $(if $(wildcard ../idl/$1.idl),\
         $(eval $(dds)_typesList = $(call typesFromDDS,../idl/$(dds).idl)),\
           $(error "NO IDL nor mIDL FILE found for $(dds)") \
   )  ) \
   $(eval $(call acsMakeIDLDependencies,$(dds),idl)) \
)  )

$(foreach dds,$(DDS_FILES),\
 $(foreach type,$($(dds)_typesList),\
   $(eval $(call acsMakeLibraryDependencies,,$(type)TypeSupportStubs,$(type)TypeSupportS $(type)TypeSupportImpl $(type)TypeSupportC,,,$(dds)Stubs) ) ) )

$(foreach dds,$(DDS_FILES),\
  $(if $(wildcard ../idl/$(dds).midl),\
    $(eval $(call acsMakeDDSDependencies,$(dds),midl)), \
    $(eval $(call acsMakeDDSDependencies,$(dds),idl))   \
  ) \
)


#################################################################
## LIB
#################################################################
ifeq ($(call mustBuild,C++),true)

LIBRARY_LIST = $(LIBRARIES) $(LIBRARIES_L)

ifneq ($(strip $(LIBRARY_LIST)),)
$(eval $(call top-level,lib,$(LIBRARY_LIST),$(LIBRARIES)))
else
$(eval $(call fake-top-level,lib))
endif


ifneq "$(strip $(LIBRARY_LIST))" "" 

$(foreach lib,$(LIBRARY_LIST),\
  $(eval \
  $(call acsMakeLibraryDependencies,$(VW),$(lib),$($(lib)_OBJECTS),$($(lib)_LDFLAGS),$($(lib)_NOSHARED),$($(lib)_LIBS) ) \
 ) \
)
endif # 
endif
#################################################################
## EXE
#################################################################

ifeq ($(call mustBuild,C++),true)
EXECUTABLE_LIST =  $(EXECUTABLES) $(EXECUTABLES_L)
$(eval $(call top-level,exe,$(EXECUTABLE_LIST),$(EXECUTABLES)))

ifneq "$(strip $(EXECUTABLE_LIST))" "" 
$(foreach exe,$(EXECUTABLE_LIST),\
 $(eval \
  $(call acsMakeExecutableDependencies,$(VW),$(exe),$($(exe)_OBJECTS),$($(exe)_LDFLAGS),$($(exe)_NOSHARED),$($(exe)_LIBS) ) \
 ) \
)
endif
endif
#################################################################
## PYTHON
#################################################################
.PHONY : clean_python
.PHONY : install_python
.PHONY : install_pythondoc
.PHONY : do_pythonscripts

ifndef MAKE_VXWORKS
ifeq ($(call mustBuild,Python),true)
ifneq ($(strip $(join $(PY_MODULES), $(PY_PACKAGES),$(PY_SCRIPTS)) ),)
    INSTALL_TARGET := $(INSTALL_TARGET) install_pythondoc
endif
endif # must build Python
endif

PY_MOD_LIST = $(PY_MODULES) $(PY_MODULES_L)  
PY_PACK_LIST = $(PY_PACKAGES) $(PY_PACKAGES_L)  
PY_SCRIPT_LIST = $(PY_SCRIPTS) $(PY_SCRIPTS_L)
PY_ALL_LIST = $(PY_SCRIPTS) $(PY_SCRIPTS_L) $(PY_PACKAGES) $(PY_PACKAGES_L)  $(PY_MODULES) $(PY_MODULES_L)  
#

ifneq ($(strip $(PY_PACK_LIST)),)
$(eval $(call top-level,python_package,$(PY_PACK_LIST),$(PY_PACKAGES)))
$(foreach pp,$(PY_PACK_LIST),$(eval $(call acsMakePythonPackageDependencies,$(pp))))
endif
#
ifneq ($(strip $(PY_MOD_LIST)),)
$(eval $(call top-level,python_module,$(PY_MOD_LIST),$(PY_MODULES)))
$(foreach pm,$(PY_MOD_LIST),$(eval $(call acsMakePythonModDependencies,$(pm))))

# backwards compatibility
do_pythonmod: do_python_modules
	@$(call obsolete,$@)

endif
#
ifneq ($(strip $(PY_SCRIPT_LIST)),)
$(eval $(call top-level,python_script,$(PY_SCRIPT_LIST),$(PY_SCRIPTS)))
$(foreach ps,$(PY_SCRIPT_LIST),$(eval $(call acsMakePythonScriptDependencies,$(ps))))
endif


do_pythondoc:
ifneq ($(strip $(PY_ALL_LIST)),)
	-$(AT) echo "Pydoc..."
	-$(AT) mkdir -p $(PYTHON_DOCS)
	-$(AT) PYTHONPATH=${PYTHONPATHDOC}; for member in $(foreach name, $(PY_SCRIPTS), $(name) ); \
	       do \
		(cd $(PYTHON_DOCS) ;  pydoc -w $${member} ); \
	       done
	-$(AT) PYTHONPATH=${PYTHONPATHDOC}; for member in $(foreach name, $(PY_MODULES), $(name) ); \
	       do \
		(cd $(PYTHON_DOCS) ;  pydoc -w $${member} ); \
	       done
	-$(AT) PYTHONPATH=${PYTHONPATHDOC}; for member in $(foreach name, $(PY_PACKAGES), $(name) ); \
	       do \
		PYFILES=`find $${member} | tr '\/' '.' |  sed 's/\.py//;' | egrep -v '\.CVS|\.\.svn'`; export PYFILES;  if [ "$$PYFILES" != "" ] ; then \
		      for pyfile in  $$PYFILES ; \
		      do \
			(cd $(PYTHON_DOCS); pydoc -w $${pyfile}) ; \
		      done ;\
	       fi ; \
               done
endif	

#################################################################
## PANELS
#################################################################
PANEL_LIST = $(PANELS) $(PANELS_L)
#
# if the list of panels is not empty, include panel-dependencies files. 
ifneq ($(strip $(PANEL_LIST)),)
$(eval $(call top-level,panel,$(PANEL_LIST),$(PANELS)))
$(foreach panel,$(PANEL_LIST), $(eval $(call acsMakePanelDependencies,$(panel)) ) )
endif

#################################################################
## DBL_CLASSES DBL_BCF
#################################################################

ifneq ($(strip $(DBL_CLASSES)),)
install_DB-Classes: DB-Classes_begin $(foreach dblc, $(DBL_CLASSES), $(DBL)/$(dblc).class )

DB-Classes_begin:
	@$(ECHO) "...DB-Classes:"

$(DBL)/%.class: ../dbl/%.class
	-$(AT)echo "\t$*" 
	$(AT)cp ../dbl/$*.class $(DBL)/$*.class
	$(AT)chmod $(P644) $(DBL)/$*.class

endif

ifneq ($(strip $(DBL_BCF)),)
install_DB-BCF: DB-BCF_begin $(foreach dblb, $(DBL_BCF), $(DBL)/$(dblb).db )

DB-BCF_begin:
	@echo "...DB-Classes:"

$(DBL)/%.db: ../dbl/%.db
	-$(AT)echo "\t$*"
	$(AT)cp ../dbl/$*.db $(DBL)/$*.db
	$(AT)chmod $(P644) $(DBL)/$*.db
endif


#################################################################
## TCL/SCRIPTS  TCL/LIBRARIES
#################################################################

TCL_SCR_LIST := $(TCL_SCRIPTS) $(TCL_SCRIPTS_L) 
TCL_LIB_LIST := $(TCL_LIBRARIES) $(TCL_LIBRARIES_L)

ifndef MAKE_VXWORKS

ifneq ($(strip $(TCL_SCR_LIST)),)
$(eval $(call top-level,tcl_script,$(TCL_SCR_LIST),$(TCL_SCRIPTS)))
$(foreach ts,$(TCL_SCR_LIST),$(eval $(call acsMakeTclScriptDependencies,$(ts),$($(ts)_OBJECTS))))
endif

ifneq ($(strip $(TCL_LIB_LIST)),)
$(eval $(call top-level,tcl_lib,$(TCL_LIB_LIST),$(TCL_LIBRARIES)))
$(foreach tl,$(TCL_LIB_LIST),$(eval $(call acsMakeTclLibDependencies,$(tl))))
endif

endif


#################################################################
## RTAI
#################################################################

##
# - for Automatic Dependencies for RTAI Modules
#
RTAI_MODULES_LIST = $(RTAI_MODULES) $(RTAI_MODULES_L)
ifneq "$(strip $(RTAI_MODULES_LIST))" "" 
$(eval $(call top-level,rtai,$(RTAI_MODULES_LIST),$(RTAI_MODULES)))

$(foreach rm,$(RTAI_MODULES_LIST), \
   $(eval $(call acsMakeExecutableDependencies,,load$(rm),load$(rm),$(load$(rm)_LDFLAGS),on,LKM C++ )))
$(foreach rm,$(RTAI_MODULES_LIST), \
   $(eval $(call acsMakeExecutableDependencies,,unload$(rm),unload$(rm),$(unload$(rm)_LDFLAGS),on,LKM C++ )))
$(foreach rm,$(RTAI_MODULES_LIST), \
   $(eval $(call acsMakeRTAIDependencies,$(rm),$($(rm)_OBJECTS) ) ) )

# enhancing the clean target further
clean_rtais: clean_rtai_final

.PHONY:
clean_rtai_final:
	$(AT)$(RM) Kbuild ../rtai/$(rtai_install_subfold)

endif
# some target needs to be assigned this task
#	-$(AT)$(RM) ../rtai/$(rtai_install_subfold)


#################################################################
## INSTALL_FILES
#################################################################

$(foreach ifile,$(wildcard $(INSTALL_FILES)), \
   $(eval $(call fileToInstall,$(ifile))))

$(foreach ifile,$(wildcard $(INSTALL_FILES)), \
   $(eval $(call acsMakeInstallFileDependencies,$(ifile))) )

install_files: files_begin $(foreach ifile,$(wildcard $(INSTALL_FILES)),install_file_$(ifile))

.PHONY:
files_begin:
	@echo "...other files"
#
#___oOo___


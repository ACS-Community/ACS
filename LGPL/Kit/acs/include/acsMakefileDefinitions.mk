#
# $Id: acsMakefileDefinitions.mk,v 1.30 2012/03/06 19:16:56 tstaig Exp $
#
#*******************************************************************************
# ALMA - Atacama Large Millimeter Array
# Copyright (c) ESO - European Southern Observatory, 2014
# (in the framework of the ALMA collaboration).
# All rights reserved.
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#*******************************************************************************

#(info Entering definitions.mk)

#
# MACRO DEFINTIONS AREA
#

#jarfile fqname, name, object dir, source dir
# this assume all jarfiles will always be located in ../lib/<something>.jar
# this also assumes that if the jarfile exists, it will be added to
# this assumes that the third argument will end with '/src'.
#
#exclude CVS files
#exclude .svn files

# procedure to return a unique list
unique = $(if $(1),$(strip $(word 1,$(1)) $(call unique,$(filter-out $(word 1,$(1)),$(1)))))
obsolete = $(ECHO) "\aThe target $(1) is *OBSOLETE* and shall be removed in future versions"
# this must be suitable for Eclipse
#

fileToInstall = \
$(eval name_$1    := $(shell basename $1)) \
$(eval fulldir_$1 := $(shell dirname  $1)) \
$(eval dir_$1     = $(if $(filter ../%,$1),$(patsubst ../%,%,$(dir $1)), $(shell basename $(fulldir_$1)))) \
$(eval toFile_$1 = $(if $(wildcard $(PRJTOP)/$(dir_$1)), \
     $(PRJTOP)/$(dir_$1)/$(name_$1), \
         $(if $(wildcard $(INSTALL_ROOT)/$(dir_$1)), \
               $(INSTALL_ROOT)/$(dir_$1)/$(name_$1), \
               $(error $(dir_$1) is not a standard directory) ) ))

isNotCurrentDir = \
$(if $(subst $(abspath $(dir $1)),,$(abspath .)),T,)

cutOffCWD = \
$(if $(call isNotCurrentDir,$1),$1,$(notdir $1))
################################################################################
# FUNCTION createJar
#
# Creates or updates (if already present) a jarfile with the .class files
# present in a given root directory. Optionally, it also includes the source
# files, if present.
#
# Arguments:
#  $1  jarfile name, without extension
#  $2  (not used?)
#  $3  directory from where the source .java files are taken from
#  $4  Whether the resulting jarfile should include the source .java files.
#      Any non-empty value will produce that source files are included
define createJar
	$(AT) jarfile="`pwd`/../lib/$(strip $1).jar"; \
           $(RM) -f $$$${jarfile}.tmp.upd ; \
           if [[ -a $$$${jarfile} ]] ; then \
             echo "== Updating jarfile $1 from $3"; \
             JAR="jar uf"; \
             cp $$$${jarfile} $$$${jarfile}.tmp.upd; \
             jarfile+=".tmp.upd"; \
           else \
             echo "== Creating jarfile $1 from $3"; \
             JAR="jar cf"; \
             origin_jarfile=$$$${jarfile}; \
             jarfile+=".tmp"; \
             if [ -f CVS/Repository ]; then \
               thisModule=`cat CVS/Repository`; \
               (cd $3 && echo "ACS-Created-From: $$$$thisModule" > $1.manifest; ); \
             else \
               if [ -d .svn ]; then \
                 thisModule=`svn info . | grep URL | awk '{print $$2}'`; \
                 (cd $3 && echo "ACS-Created-From: $$$$thisModule" > $1.manifest; ); \
               else \
                 (cd $3 && echo "ACS-Created-From: $(PWD)" > $1.manifest;) ;\
               fi; \
             fi; \
           fi; \
           cd $3 && \
           FILES=`find . -name \*.class`; \
	   tmpFile=/tmp/acsMakefileJava$(strip $1)_$(UNIQUE_NUMBER)_$(USER_NAME); \
           if [[ -n $$$${FILES} ]]; then \
             echo $$$${FILES} > $$$${tmpFile} && \
             $$$${JAR} $$$${jarfile} @$$$${tmpFile} && \
             rm $$$${tmpFile} ; \
           else  \
	     echo "WARNING, no files to add to $1"; \
             touch $$$${jarfile} ; \
           fi; \
           if [ -f $1.manifest ]; then \
             jar ufm $$$${jarfile} $1.manifest; \
           fi; \
           if [[ "$$$${origin_jarfile}" != "" ]]; then \
             mv $$$${jarfile} $$$${origin_jarfile}; \
           else \
             mv $$$${jarfile} $(PWD)/../lib/$(strip $1).jar ; \
           fi;
	$(AT)jarfile="`pwd`/../lib/$(strip $1).jar"; \
        tmpFile=/tmp/acsMakefileJava$(strip $1)_$(UNIQUE_NUMBER)_$(USER_NAME); \
        if [[ "$4" != "" ]]; then \
          cd $3/..; \
          FILES=`find src ! -path '*/CVS/*' ! -path '*/.svn/*' -name *.java`; \
          if [[ -n $$$${FILES} ]]; then \
            echo $$$${FILES} > $$$${tmpFile}; \
            jar uf $$$${jarfile} @$$$${tmpFile}; \
            rm $$$${tmpFile}; \
          fi; \
        fi

endef


# $(call IDLInstall,TO-DIR,FROM-DIR,EXT,FILE)
define IDLInstall

$(PRJTOP)/$1/$4$3: ../$2/$4$3
	$(AT)cp ../$2/$4$3 $(PRJTOP)/$1/$4$3;
	$(AT)chmod $(P755) $(PRJTOP)/$1/$4$3;

# the empty line above seems to be fundamental...
endef


debug-enter = $(if $(debug_trace),\
		$(warning Entering $0($(echo-args))))

debug-leave = $(if $(debug_trace),$(warning Leaving $0))
comma:= ,
echo-args = $(subst ' ','$(comma) ',\
		$(foreach a,1 2 3 4 5 6 7 8 9,'$($a)'))

#
# attempt to make these rules generated more easily and above all
# with 'limiters' which restrict them only to those stems contained in
#

define IDLPrereq
install_idl_$1_prereq=$(PRJTOP)/idl/$1.idl
ifeq ($(strip $(MAKE_NOIFR_CHECK)),)
do_idl_$1_prereq=do_idl_$1_irCheck
endif

ifndef MAKE_VXWORKS
ifeq ($(call mustBuild,Python),true)
 do_idl_$1_prereq += $1_IDL_Python
 install_idl_$1_prereq += install_IDL_$1_Python
 clean_idl_$1_prereq += clean_IDL_$1_Python
endif
ifeq ($(call mustBuild,Java),true)
 do_idl_$1_prereq += $1_IDL_Java
 install_idl_$1_prereq += install_IDL_$1_Java
 clean_idl_$1_prereq += clean_IDL_$1_Java
endif
endif

ifeq ($(call mustBuild,C++),true)
 do_idl_$1_prereq += $1_IDL_CPP
 install_idl_$1_prereq += install_IDL_$1_CPP
 clean_idl_$1_prereq += clean_IDL_$1_CPP
 clean_dist_idl_$1_prereq += clean_dist_IDL_$1_CPP
endif


$1_IDLprereq += $(if $(wildcard ../idl/$1.idl),$(subst ../idl/$1.idl,,$(subst $1.o:, ,$(shell cpp  $(MK_IDL_PATH) $(TAO_MK_IDL_PATH)  -E -M ../idl/$1.idl 2>/dev/null |  tr  '\\\n' ' '  ))) ,)

$1_MIDLprereq += $(if $(wildcard ../idl/$1.midl),$(subst ../idl/$1.midl,,$(subst $1.o:, ,$(shell cpp  $(MK_IDL_PATH) $(TAO_MK_IDL_PATH)  -E -M ../idl/$1.midl 2>/dev/null |  tr  '\\\n' ' '  ))) ,)


# jagonzal: Add dependencies on IDLs generated from XMLs since the preprocessor requires all the included IDLs to be available in order to produce any output
ifdef ACSERRDEF
   $1_IDLprereq += $(filter-out $(CURDIR)/../idl/$(1).idl,$(foreach xml,$(ACSERRDEF),$(CURDIR)/../idl/$(xml).idl))
   $1_MIDLprereq += $(filter-out $(CURDIR)/../idl/$(1).idl,$(foreach xml,$(ACSERRDEF),$(CURDIR)/../idl/$(xml).idl))
endif

endef

##################################################################
##################################################################

# $(call acsMakeIDLDependencies,idl-file,ext)
define acsMakeIDLDependencies


.PHONY: do_idl_$1
do_idl_$1: $(do_idl_$1_prereq)

.PHONY: install_idl_$1
install_idl_$1: $(install_idl_$1_prereq)
	@echo "Finished installing IDL $1"


.PHONY: clean_idl_$1
clean_idl_$1: $(clean_idl_$1_prereq)
	$(AT)if [ -f ../idl/$1.midl ]; then $(RM) ../idl/$1.idl; fi
	$(AT)if [ -f result.idl ]; then $(RM) result.idl; fi


.PHONY: clean_dist_idl_$1
clean_dist_idl_$1: $(clean_dist_idl_$1_prereq)

##
#########################################
## ALL
#########################################
do_idl_$1_irCheck:
	$(AT) if [ "$(MAKE_NOIFR_CHECK)" == "" ]; then  echo "== checking IDL for Interface Repository ==" ; acsstartupLoadIFR -c ../idl/$1.idl; fi
	$(AT)if [ -f result.idl ]; then $(RM) result.idl; fi

.PHONY: $1_IDL_CPP
ifdef MAKE_VXWORKS
$1_IDL_CPP: do_exe_$1Stubs;
else
$1_IDL_CPP: do_lib_$1Stubs;
endif

ifeq ($(call mustBuild,C++),true)
../object/$1C.cpp: $(CURDIR)/../idl/$1.idl $($1_IDLprereq)
	-@echo "== IDL Compiling for TAO (C++): $1"
ifeq ($(platform),Cygwin)
	$(AT) $(TAO_IDL) -Sg $(MK_IDL_PATH) $(TAO_MK_IDL_PATH) -o /tmp/ -Wb,export_macro=$1Stubs_Export -Wb,export_include=$1StubsExport.h $(TAO_IDLFLAGS) $$<
else
	$(AT) $(TAO_IDL) -Sg $(MK_IDL_PATH) $(TAO_MK_IDL_PATH) -o /tmp/ $(TAO_IDLFLAGS) $$< 
endif
	$(AT) mv -f $(foreach ext,$(IDL_EXTENSIONS),/tmp/$1$(ext)) ../object/

# this is a trick which I am trying to use for parallelization
# this makes it unfortunately less maintainable, but easier to read and understand
# than if using $(word 1,$(foreach ext,$(filter %.cpp,$(IDL_EXTENSIONS)),../object/$1$(ext)))
# and such things.
../object/$1S.cpp: ../object/$1C.cpp;

endif

# Note: this assumes file already available early on, and not generated
# by a make target (like do_links)
$(if $(wildcard ../idl/$1.midl),
$(CURDIR)/../idl/$1.idl: ../idl/$1.midl $($1_MIDLprereq)
	-@echo "== (preprocessing MIDL => IDL) $1"
	$(AT) JacPrep $$< " -I$(JACORB_HOME)/idl/jacorb -I$(JACORB_HOME)/idl/omg $(MK_IDL_PATH) $(MIDL_FLAGS)" >  ../idl/$1.idl
)

.PHONY: $1_IDL_Java
$1_IDL_Java: TMPSRC:= ../object/$1/src
$1_IDL_Java: $(CURDIR)/../lib/$1.jar;


$(CURDIR)/../lib/$1.jar: TMPSRC=../object/$1/src
$(CURDIR)/../lib/$1.jar: $(CURDIR)/../idl/$1.idl $($1_IDLprereq) $(subst ../idl/,,$(subst .pidl,.jar,$(subst .idl,.jar,$(foreach idl,$($1_IDLprereq),$(if $(wildcard ../idl/$(idl)),$(CURDIR)/../lib/$(idl), ))  ))) $(if $(filter $1,$(ACSERRDEF)),../idl/$1.xml,)
	- @echo "== (preprocessing) $1"
	$(AT) JacPrep $$< " -I$(JACORB_HOME)/idl/jacorb -I$(JACORB_HOME)/idl/omg $(MK_IDL_PATH) " >  /tmp/$(UNIQUE_NUMBER).$1.idl
	- @echo "== IDL Compiling for JacORB (Java): $1 "
	$(AT) mkdir -p $$(TMPSRC) ; if [ -s /tmp/$(UNIQUE_NUMBER).$1.idl ]; then \
	$$(JAVA_IDL) -auto_prefix   $$(JACORB_MK_IDL_PATH) $$(MK_IDL_PATH) -d $$(TMPSRC)   /tmp/$(UNIQUE_NUMBER).$1.idl ;\
	else \
           echo "File /tmp/$(UNIQUE_NUMBER).$1.idl not found, JacPrep FAILED";\
           /bin/false; \
	fi; 
	$(AT) $(RM) /tmp/$(UNIQUE_NUMBER).$1.idl
ifneq ($(strip $(XML_IDL)),)
	- @echo "== IDL Compiling for XmlIdl  (Java): $1"
ifdef ACSROOT
	$(AT) CLASSPATH="`acsMakeJavaClasspath`$(PATH_SEP)$(ACSROOT)/lib/endorsed/xercesImpl.jar" ;  export CLASSPATH; $(XMLIDL) $(MK_IDL_PATH) $(JACORB_MK_IDL_PATH) $$<
else
	$(AT) CLASSPATH="`acsMakeJavaClasspath`$(PATH_SEP)$(INSTALL_ROOT_LAST)/lib/endorsed/xercesImpl.jar" ;  export CLASSPATH; $(XMLIDL) $(MK_IDL_PATH) $(JACORB_MK_IDL_PATH) $$<
endif #end ACSROOT
endif #end $(strip $(XML_IDL))
	$(AT) CLASSPATH="`acsMakeJavaClasspath`"; \
        export CLASSPATH; \
        FILES=`find $$(TMPSRC) ! -path '*/CVS/*' ! -path '*/.svn/*' -name \*.java`; \
        tmpFile=/tmp/acsMakeJavaStubs$(strip $1)_$(UNIQUE_NUMBER)_$(USER_NAME); \
        if [ "$$$${FILES}" != "" ] ; then \
          echo $$$${FILES} > $$$${tmpFile}; \
          $(JAVAC) $(javaCompilerOptions) $(JAVA_EDIRS) -d $$(TMPSRC) @$$$${tmpFile} || { $(RM) $$$${tmpFile} && exit 3 ; } ; \
          $(RM) $$$${tmpFile} ; \
        else \
          echo "== WARNING, no Java source files generated for $1"; \
        fi
ifeq ($(strip $(DEBUG)),on)
	$(call createJar,$1, $1, $$(TMPSRC),on)
else
	$(call createJar,$1, $1, $$(TMPSRC),)
endif # DEBUG
	$(AT) $(RM) ../object/$1
ifeq ($(filter $1,$(ACSERRDEF)),$1)
#### XMLERR part here
	$(AT)mkdir -p $(tDir)-$1/src
	$(AT)echo "== Generating Java classes from ../idl/$1.xml"
	$(AT)acserrGenJava ../idl/$1.xml $(tDir)-$1/src
	$(AT)echo "== Compiling generated Java classes (ACSERRDEF) and adding them to ../lib/$1.jar"
ifeq ($(os),$(CYGWIN_VER))
	$(AT)(cd $(tDir)-$1 && mkdir lib && cp ../../lib/*jar lib; cd src;  export CLASSPATH="`acsMakeJavaClasspath`$(PATH_SEP)."; $(JAVAC) $(JAVA_EDIRS) -d . `find . -name \*.java | tr '\n' ' '` ) || exit -13
else
	$(AT)(cd $(tDir)-$1 && ln -s ../../lib lib &&  cd src;  export CLASSPATH="`acsMakeJavaClasspath`$(PATH_SEP)."; $(JAVAC) $(JAVA_EDIRS) -d . `find . -name \*.java | tr '\n' ' '` ) || exit -13
endif
ifeq ($(strip $(DEBUG)),on)
	$(call createJar,$1, $1ACSERRDEF, $(tDir)-$1/src,$(tDir)-$1/src)
else
	$(call createJar,$1, $1ACSERRDEF, $(tDir)-$1/src,)
endif #DEBUG
	$(AT)rm -fr $(tDir)-$1
#### XMLERR part here
endif # XMLERR


# C++ CLEAN
.PHONY: clean_IDL_$1_CPP
ifdef MAKE_VXWORKS
clean_IDL_$1_CPP: clean_exe_$1Stubs
else
clean_IDL_$1_CPP: clean_lib_$1Stubs
endif #end MAKE_VXWORKS
	$(AT)$(ECHO) "\t$1 generated C++ code"
	$(AT) $(RM) $(foreach ext,$(IDL_EXTENSIONS), ../object/$1$(ext)) $(foreach ext,$(patsubst %.cpp,%.o,$(filter %.cpp,$(IDL_EXTENSIONS))), ../object/$1$(ext))

.PHONY: clean_dist_IDL_$1_CPP
clean_dist_IDL_$1_CPP: clean_IDL_$1_CPP

.PHONY: clean_IDL_$1_Java
clean_IDL_$1_Java:
	$(AT) $(RM) ../lib/$1.jar


# 
# Python
# 
.PHONY: $1_IDL_Python
$1_IDL_Python: ../lib/python/site-packages/$1_idl.py ; 

../lib/python/site-packages/$1_idl.py: $(CURDIR)/../idl/$1.idl $($1_IDLprereq)
	$(AT)lockfile -s 2 -r 10 ../lib/python/site-packages/.make-OmniOrb.lock || echo "WARNING, ignoring lock ../lib/python/site-packages/.make-OmniOrb.lock"
	- @echo "== IDL Compiling for OmniOrb (Python): $1"
	$(AT) $(OMNI_IDL)  -I$(OMNI_ROOT)/idl/ $(MK_IDL_PATH) $(TAO_MK_IDL_PATH) -bacs_python -C../lib/python/site-packages $$<
	-$(AT)$(RM) ../lib/python/site-packages/.make-OmniOrb.lock

.PHONY: clean_IDL_$1_Python
clean_IDL_$1_Python:
	$(AT)$(RM)  ../lib/python/site-packages/$1
	$(AT)$(RM)  ../lib/python/site-packages/$1__POA
	$(AT)$(RM)  ../lib/python/site-packages/{$1_idl.py,$1_idl.pyc}

#
# INSTALL
#

.PHONY: install_IDL_$1_Python
install_IDL_$1_Python: $(CURDIR)/../idl/$1.idl
	$(AT)lockfile -s 2 -r 10 $(LIB)/python/site-packages/.make-OmniOrb.lock || echo "WARNING, ignoring lock $(LIB)/python/site-packages/.make-OmniOrb.lock"
	$(AT)$(OMNI_IDL)  -I$(OMNI_ROOT)/idl/ $(MK_IDL_PATH) $(TAO_MK_IDL_PATH) -bacs_python -C$(LIB)/python/site-packages $$< > /dev/null 2>&1
	-$(AT)$(RM) $(LIB)/python/site-packages/.make-OmniOrb.lock

.PHONY: install_IDL_$1_Java
install_IDL_$1_Java: $(LIB)/$1.jar;

$(LIB)/$1.jar: $(CURDIR)/../lib/$1.jar
	$(AT)cp ../lib/$1.jar $(LIB)/$1.jar
	$(AT)chmod $(P755) $(LIB)/$1.jar

.PHONY: install_IDL_$1_CPP
ifdef MAKE_VXWORKS
install_IDL_$1_CPP: install_exe_$1Stubs;
else
install_IDL_$1_CPP: install_IDL_$1_CPP_Sources install_lib_$1Stubs;
endif

$(call IDLInstall,idl,idl,.idl,$1)
# notice that the above will install the IDL file in a $(VW), if $(PRJTOP) is 
# so define
$(foreach ext,$(IDL_EXTENSIONS),$(call IDLInstall,include,object,$(ext),$1))

.PHONY: install_IDL_$1_CPP_Sources
install_IDL_$1_CPP_Sources: $(foreach ext, $(IDL_EXTENSIONS) , $(PRJTOP)/include/$1$(ext))

endef

#
#

#############################################################
#############################################################


# $(call XMLPrereq,xml)
define XMLPrereq
# this we need anyway
do_xmlerr_$1_prereq=$(CURDIR)/../idl/$1.idl do_idl_$1
clean_xmlerr_$1_prereq=clean_idl_$1
clean_dist_xmlerr_$1_prereq=clean_dist_idl_$1
install_xmlerr_$1_prereq=install_idl_$1

ifeq ($(call mustBuild,Python),true)
ifndef MAKE_VXWORKS
 do_xmlerr_$1_prereq += do_xmlerr_Python_$1
 install_xmlerr_$1_prereq += install_xmlerr_$1_Python
 clean_xmlerr_$1_prereq += clean_xmlerr_$1_Python
endif
endif
ifeq ($(call mustBuild,Java),true)
ifndef MAKE_VXWORKS
 do_xmlerr_$1_prereq += do_xmlerr_Java_$1
 install_xmlerr_$1_prereq += install_xmlerr_$1_Java
 clean_xmlerr_$1_prereq += clean_xmlerr_$1_Java
endif
endif
ifeq ($(call mustBuild,C++),true)
 do_xmlerr_$1_prereq += do_xmlerr_CPP_$1
 install_xmlerr_$1_prereq += install_xmlerr_$1_CPP
 clean_xmlerr_$1_prereq += clean_xmlerr_$1_CPP
 clean_dist_xmlerr_$1_prereq += clean_dist_xmlerr_$1_CPP
endif

endef

#############################################################
#############################################################


# $(call acsMakeXMLErrDependencies,xml)
define acsMakeXMLErrDependencies

.PHONY: check_xmlerr_$1
check_xmlerr_$1:
	$(AT)acserrGenCheckXML ../idl/$1.xml

.PHONY: do_xmlerr_$1
do_xmlerr_$1: check_xmlerr_$1 $(do_xmlerr_$1_prereq)

$(CURDIR)/../idl/$1.idl: ../idl/$1.xml
	$(AT)echo "=== Generating IDL from XMLERR definitions $1"
	$(AT)acserrGenIDL ../idl/$1.xml ../idl/$1.idl

.PHONY: do_xmlerr_CPP_$1
ifdef MAKE_VXWORKS
do_xmlerr_CPP_$1: do_exe_$1 ../object/$1.cpp ../object/$1.h
else
do_xmlerr_CPP_$1: do_lib_$1 ../object/$1.cpp ../object/$1.h
endif

../object/$1.cpp ../object/$1.h: ../idl/$1.xml
	$(AT)acserrGenCpp ../idl/$1.xml ../object/$1.cpp ../object/$1.h 


.PHONY: clean_xmlerr_$1_CPP
ifdef MAKE_VXWORKS
clean_xmlerr_$1_CPP: clean_idl_$1 clean_exe_$1
else
clean_xmlerr_$1_CPP: clean_idl_$1 clean_lib_$1
endif
	$(AT) $(RM) ../object/$1.o ../object/$1.cpp ../object/$1.h

.PHONY: clean_dist_xmlerr_$1_CPP
clean_dist_xmlerr_$1_CPP:
	$(AT) $(RM) ../object/$1.cpp ../object/$1.h
	$(AT) $(RM) ../object/$1.da

.PHONY: install_xmlerr_$1_CPP
ifdef MAKE_VXWORKS
install_xmlerr_$1_CPP: install_exe_$1 install_IDL_$1_CPP install_xmlerr_$1_CPP_generated
else
install_xmlerr_$1_CPP: install_lib_$1 install_IDL_$1_CPP install_xmlerr_$1_CPP_generated
endif


.PHONY:install_xmlerr_$1_CPP_generated
install_xmlerr_$1_CPP_generated:
	$(AT)cp ../object/$1.h $(PRJTOP)/include/

.PHONY: do_xmlerr_Python_$1
do_xmlerr_Python_$1: ../lib/python/site-packages/$1Impl.py


../lib/python/site-packages/$1Impl.py: ../object/$1Impl.py
	$(AT)cp ../object/$1Impl.py ../lib/python/site-packages/$1Impl.py

../object/$1Impl.py: ../idl/$1.xml
	$(AT)acserrGenPython ../idl/$1.xml ../object/$1Impl.py || exit -2

.PHONY: clean_xmlerr_$1_Python
clean_xmlerr_$1_Python:
	$(AT)$(RM)  ../lib/python/site-packages/$1
	$(AT)$(RM)  ../lib/python/site-packages/$1__POA
	$(AT)$(RM)  ../lib/python/site-packages/{$1Impl.py,$1Impl.pyc} ../object/$1Impl.py 

.PHONY: install_xmlerr_$1_Python
install_xmlerr_$1_Python:$(LIB)/python/site-packages/$1Impl.py

$(LIB)/python/site-packages/$1Impl.py: ../lib/python/site-packages/$1Impl.py
	$(AT)cp ../lib/python/site-packages/$1Impl.py $(LIB)/python/site-packages
	$(AT)chmod $(P644) $(LIB)/python/site-packages/$1Impl.py
	$(AT)python $(PYTHON_ROOT)/lib/python$(PYTHON_VERS)/compileall.py $(PYTHON_OUTPUT) ../lib/python/site-packages 
	$(AT)cp ../lib/python/site-packages/$1Impl.pyc $(LIB)/python/site-packages
	$(AT)chmod $(P644) $(LIB)/python/site-packages/$1Impl.pyc

.PHONY: do_xmlerr_Java_$1
do_xmlerr_Java_$1: $(CURDIR)/../lib/$1.jar

.PHONY: clean_xmlerr_$1_Java
clean_xmlerr_$1_Java:

.PHONY: install_xmlerr_$1_Java
install_xmlerr_$1_Java: $(LIB)/$1.jar


#
# CLEAN
#
.PHONY: clean_xmlerr_$1
clean_xmlerr_$1: $(clean_xmlerr_$1_prereq)

.PHONY: clean_dist_xmlerr_$1
clean_dist_xmlerr_$1: $(clean_dist_xmlerr_$1_prereq)
	$(AT)$(RM) ../idl/$1.idl 


.PHONY: install_xmlerr_$1
install_xmlerr_$1: begin_install_xmlerr_$1 $(install_xmlerr_$1_prereq)
	$(AT)cp ../idl/$1.xml $(PRJTOP)/idl

.PHONY: begin_install_xmlerr_$1
begin_install_xmlerr_$1:
	$(AT)echo "installing XMLERR derived files for $1"

endef


#############################################################
#############################################################
define XSDPrereq

 install_xsdbind_$1_prereq=$(PRJTOP)/idl/$1.xml 
ifeq ($(call mustBuild,Python),true)
 do_xsdbind_$1_prereq += do_xsdbind_Python_$1
 install_xsdbind_$1_prereq += install_xsdbind_$1_Python
 clean_xsdbind_$1_prereq += clean_xsdbind_$1_Python
endif
ifeq ($(call mustBuild,Java),true)
 do_xsdbind_$1_prereq += do_xsdbind_Java_$1
 install_xsdbind_$1_prereq += install_xsdbind_$1_Java
 clean_xsdbind_$1_prereq += clean_xsdbind_$1_Java
endif
endef

#############################################################
#############################################################

# $(call acsMakeXSDDependencies,xsd,xsd_files)
define acsMakeXSDDependencies

.PHONY: do_xsdbind_$1
do_xsdbind_$1: $(do_xsdbind_$1_prereq)

ifeq ($(os),$(CYGWIN_VER))
XSDDEPLIST = java -cp "$(ACSROOT)/lib/xalan.jar$(PATH_SEP)$(ACSROOT)/lib/xalan_serializer.jar" org.apache.xalan.xslt.Process -XSL $(MAKEDIR)/../config/XSDIncludeDependencies.xml  -IN ../idl/$1.xml  |  egrep -v \'^\ *\$$\' | tr '\r\n' ' '
else
XSDDEPLIST = java -cp "$(ACSROOT)/lib/xalan.jar$(PATH_SEP)$(ACSROOT)/lib/xalan_serializer.jar" org.apache.xalan.xslt.Process -XSL $(MAKEDIR)/../config/XSDIncludeDependencies.xml  -IN ../idl/$1.xml  |  egrep -v \'^\ *\$$\' | tr '\n' ' '
endif
#XSDBIND_FILES:= (shell $(XSDDEPLIST))

.PHONY: do_xsdbind_Python_$1
do_xsdbind_Python_$1: ../lib/python/site-packages/$1

../lib/python/site-packages/$1: ../idl/$1.xml
	-@echo "== XSD Compiling with pyxbgen (Python): $1"
	-$(AT)generateXsdPythonBinding $1
	-$(AT)cd ../lib/python/site-packages && python $(PYTHON_ROOT)/lib/python$(PYTHON_VERS)/compileall.py $(PYTHON_OUTPUT) $1

.PHONY: clean_xsdbind_$1_Python
clean_xsdbind_$1_Python:
	$(AT)$(RM) -fr ../lib/python/site-packages/$1
	$(AT)$(RM) ../lib/python/site-packages/$1.wxs

.PHONY: install_xsdbind_$1_Python
install_xsdbind_$1_Python:$(LIB)/python/site-packages/$1 $(LIB)/python/site-packages/$1.wxs

$(LIB)/python/site-packages/$1: ../lib/python/site-packages/$1
	-$(AT)cp -pr ../lib/python/site-packages/$1 $(LIB)/python/site-packages
	-$(AT)chmod $(P755) $(LIB)/python/site-packages/$1
	-$(AT)python $(PYTHON_ROOT)/lib/python$(PYTHON_VERS)/compileall.py $(PYTHON_OUTPUT) ../lib/python/site-packages

$(LIB)/python/site-packages/$1.wxs: ../lib/python/site-packages/$1.wxs
	$(AT)echo "\t$1.wxs";
	$(AT)cp ../lib/python/site-packages/$1.wxs $(LIB)/python/site-packages   
	$(AT)chmod $(P644) $(LIB)/python/site-packages/$1.wxs

.PHONY: do_xsdbind_Java_$1
do_xsdbind_Java_$1: $(CURDIR)/../lib/$1.jar

$(CURDIR)/../lib/$1.jar: TMPSRC=../object/$1/src
$(CURDIR)/../lib/$1.jar: ../idl/$1.xml
	-@echo "== XSD Compiling with Castor (Java): $1"
	$(AT) mkdir -p $$(TMPSRC)
ifdef ACSROOT
	$(AT) CLASSPATH=`acsMakeJavaClasspath`:$(ACSROOT)/lib/endorsed/xercesImpl.jar;  export CLASSPATH; java -DACS.schemaconfigfiles="$2" $(CASTOR)  ../idl/$1.xml $$(TMPSRC) $(MK_IDL_PATH)
else
	$(AT) CLASSPATH=`acsMakeJavaClasspath`:$(INSTALL_ROOT_LAST)/lib/endorsed/xercesImpl.jar;  export CLASSPATH; java -DACS.schemaconfigfiles="" $(CASTOR)  ../idl/$1.xml $$(TMPSRC) $(MK_IDL_PATH)	
endif #ACSROOT
	$(AT) CLASSPATH=`acsMakeJavaClasspath`; export CLASSPATH; FILES=`find $$(TMPSRC) -name \*.java`; export FILES;  if [ "$$$${FILES}" != "" ] ; then $(JAVAC) $(javaCompilerOptions) $(JAVA_EDIRS) -d $$(TMPSRC) $$$${FILES} || exit -13 ; fi;
ifeq ($(strip $(DEBUG)),on)
	$(call createJar,$1, $1, $$(TMPSRC),on)
else
	$(call createJar,$1, $1, $$(TMPSRC),)
endif # DEBUG
	$(AT) $(RM) $$(TMPSRC)

.PHONY: clean_xsdbind_$1_Java
clean_xsdbind_$1_Java:
	$(AT) $(RM) ../lib/$1.jar

.PHONY: install_xsdbind_$1_Java
install_xsdbind_$1_Java: $(LIB)/$1.jar

$(LIB)/$1.jar: $(CURDIR)/../lib/$1.jar
	$(AT)cp ../lib/$1.jar $(LIB)/$1.jar
	$(AT)chmod $(P755) $(LIB)/$1.jar


#
# CLEAN
#
.PHONY: clean_xsdbind_$1
clean_xsdbind_$1: $(clean_xsdbind_$1_prereq)
	$(AT)$(RM) ../object/$1.dxsd 
	$(AT)if [ -d ../object/$1 ]; then $(RM) ../object/$1; fi

.PHONY: clean_dist_xsdbind_$1
clean_dist_xsdbind_$1: clean_xsdbind_$1;


.PHONY: install_xsdbind_$1
install_xsdbind_$1: begin_install_xsdbind_$1 $(install_xsdbind_$1_prereq)

.PHONY: begin_install_xsdbind_$1
begin_install_xsdbind_$1:
	$(AT)echo "installing XSDBIND derived files for $1"

$(PRJTOP)/idl/$1.xml: ../idl/$1.xml
	-$(AT) cp ../idl/$1.xml $(PRJTOP)/idl/   
	-$(AT) cp ../idl/*.xsd $(PRJTOP)/idl/   
	-$(AT) chmod $(P755) $(PRJTOP)/idl/$1.xml

endef

#############################################################
#############################################################

define JARPrereq
ifeq ($(call mustBuild,Java),true)
 do_java_$1_prereq = do_java_Java_$1
 install_java_$1_prereq=install_java_$1_Java
 clean_java_$1_prereq= clean_java_$1_Java
endif
endef

#############################################################
#############################################################

# $(call acsMakeJavaDependencies,jar,jar_dirs,jar_extras,comp_jar,userCompilerFlags,jarCompilerFlags,endorsed,debug,libs)

define acsMakeJavaDependencies

$(debug-enter)

# notice that any info command like the one below will be executed whenever a new target is going to be remade, I do not know yet why. MZA

tgtDir$1=$(if $(filter on,$4),lib/ACScomponents,lib)
CompilerFlags=$(if $(filter on,$8),$5 $6 -g,$(shell echo $5 $6 | sed -e 's/-g //;s/-g[:a-z,]*//'))
currentLocation=$(if $1,$(shell pwd | sed "s/\/.*\///"))

classMaker=acsMakeJavaClasspath
J_END_DIRS=$(if $(filter on,$7),$(JAVA_EDIRS),$(JACORB_ENDORSED))
$1_FILELISTFILE=/tmp/acsMakeJavac_$1_$(UNIQUE_NUMBER)_$(USER_NAME)

$1_source_file_list= $(if $2,$(shell find $2 -name \*.java -type f ! -path '*/CVS/*' ! -path '*/.svn/*' | tr '\n' ' '),)
$1_class_file_list= $(subst .java,.class,$(foreach src,$$($1_source_file_list),$(src)) )
$(if $1,$(eval $1_sourcePrefixed_file_list=$(foreach jFile,$$($1_source_file_list),$$(currentLocation)/$(jFile))),)


.PHONY: do_java_$1
do_java_$1: $(do_java_$1_prereq)

.PHONY: do_java_Java_$1
do_java_Java_$1: $(CURDIR)/../$$(tgtDir$1)/$1.jar


$(CURDIR)/../$$(tgtDir$1)/$1.jar: TMPSRC=../object/$1/src 
$(CURDIR)/../$$(tgtDir$1)/$1.jar: $$($1_source_file_list) $(foreach jar,$9,$(CURDIR)/../lib/$(jar).jar)
	@echo "== Making Jarfile $1.jar"
	$(AT) $(RM) $$($1_FILELISTFILE)
	$(AT) mkdir -p $$(TMPSRC)
	$(AT) for f in $$(filter %.java,$$?); do echo $$$$f >> $$($1_FILELISTFILE); done
	$(AT) if [ -f CVS/Repository ]; then \
	   echo "$(strip $1)-ACS-Generated-FromModule: `cat CVS/Repository`" >  ../object/$1/$(strip $1).manifest; \
           else \
             if  [ -d .svn ]; then \
               thisModule=`svn info . | grep URL | awk '{print $$2}'`; \
               echo "$(strip $1)-ACS-Generated-FromModule: $$$$thisModule" >  ../object/$1/$(strip $1).manifest; \
             else \
               echo "$(strip $1)-ACS-Generated-FromModule: $(PWD)" >  ../object/$1/$(strip $1).manifest; \
             fi; \
        fi
	$(AT) CLASSPATH="`$$(classMaker)`$(PATH_SEP)."; export CLASSPATH ; $(JAVAC) $$(CompilerFlags) $$(J_END_DIRS) -d $$(TMPSRC) @$$($1_FILELISTFILE)
	$(AT) (cd $$(TMPSRC); if [ -f ../../../$$(tgtDir$1)/$1.jar ]; then JAR="jar uf"; else JAR="jar cf"; fi; $$$${JAR} ../../../$$(tgtDir$1)/$1.jar $(foreach dir,$2,$(if $(filter .,$(dir)),*.class,$(dir))) && jar ufm ../../../$$(tgtDir$1)/$1.jar ../$(strip $1).manifest 2> /dev/null && $(RM) ../$(strip $1).manifest )
	$(AT) $(RM) -fr $$(TMPSRC)
	$(AT) $(RM) $$($1_FILELISTFILE)
ifeq ($(strip $(DEBUG)),on)
	$(AT)cd .. && jar uf $$(tgtDir$1)/$1.jar `for f in $$($1_source_file_list); do echo $$(currentLocation)/$$$${f} | tr '\n' ' '; done`
endif # DEBUG
	$(AT)$(if $3,jar uf ../$$(tgtDir$1)/$1.jar $3,)

.PHONY: clean_java_$1_Java
clean_java_$1_Java:
	$(AT) $(RM) ../$$(tgtDir$1)/$1.jar
	$(AT) if [ -f $1-restart.mk ]; then $(RM) $1-restart.mk; fi

.PHONY: install_java_$1_Java
install_java_$1_Java: $(PRJTOP)/$$(tgtDir$1)/$1.jar

$(PRJTOP)/$$(tgtDir$1)/$1.jar: $(CURDIR)/../$$(tgtDir$1)/$1.jar
	$(AT)cp ../$$(tgtDir$1)/$1.jar $(PRJTOP)/$$(tgtDir$1)/$1.jar
	$(AT)chmod $(P755) $(PRJTOP)/$$(tgtDir$1)/$1.jar

#
# CLEAN
#
.PHONY: clean_java_$1
clean_java_$1: $(clean_java_$1_prereq)
	$(AT)if [ -d ../object/$1 ]; then rm -fr ../object/$1; fi

.PHONY: clean_dist_java_$1
clean_dist_java_$1:clean_java_$1;

.PHONY: install_java_$1
install_java_$1: begin_install_java_$1 $(install_java_$1_prereq)

.PHONY: begin_install_java_$1
begin_install_java_$1:
	$(AT)echo "installing jarfile $1"

$(debug-leave)
endef

#############################################################
#############################################################
ifeq ($(platform),Cygwin)
deps = $(foreach dep, $(if $(strip $($1_lList)),$(patsubst -l%,%,$($1_lList) $($1_dList)),$(if $(wildcard $(shell searchFile lib/lib$1.dll.dep)/lib/lib$1.dll.dep),$(strip $(shell cat $(shell searchFile lib/lib$1.dll.dep)/lib/lib$1.dll.dep)),)), -l$(dep))
endif

#acsMakeLibraryDependencies $(VW),$(*F),$($(*F)_OBJECTS),$($(*F)_LDFLAGS),$($(*F)_NOSHARED),$($(*F)_LIBS)

# $(call acsMakeLibraryDependencies 
define acsMakeLibraryDependencies

#ifeq ($(strip $(MAKE_UNIX)),TRUE)
$(if $(filter /vw,$1),,
 $2_sharedLib=$(CURDIR)/../lib/lib$2.$(SHLIB_EXT)
 $2_sharedLibName=-Xlinker -h -Xlinker lib$2.$(SHLIB_EXT))
#endif

$2_depList =
$2_expObjList  =
$(foreach obj,$3,$(eval $$2_depList += ../object/$(obj).d))
$(foreach obj,$3,$(eval $$2_expObjList += ../object/$(obj).o))

clean_dependencies_target += $($2_depList)

ifeq ($(DEPENDENCIES),on)
   ifeq ($(MAKE_DEBUG),on)
      include $($2_depList)
   else
      -include $($2_depList)
   endif
endif

.PHONY: do_lib_$2 
.PHONY: clean_lib_$2 
.PHONY: clean_dist_lib_$2

$(if $(or $3,$6), \
	$(if $(findstring Cygwin, $(platform)), \
	  xyz_$2_OBJ = $(foreach obj,$3,../object/$(obj).o$(eval $(obj)_CFLAGS += $($2_CFLAGS))), \
	  xyz_$2_OBJ = $(foreach obj,$3,../object/$(obj).o) ) \
	$(foreach lib, $6, \
            $(if $(filter stdc++,$(lib)), \
	        $(eval $2_lList += -l$(lib)  MESSAGE += somemessage ), \
                $(if $(filter g++,$(lib)), \
                    $(eval MESSAGE += ecgs does not provide), \
                    $(if $(filter iostream,$(lib)), \
                        $(eval MESSAGE += please remove iostream), \
	                $(if $(findstring Cygwin, $(platform)), \
	                    $(eval $2_lList += -l$(lib) $(eval $2_dList += $(call deps,$(lib)))), \
                            $(eval $2_lList += -l$(lib)) 
                         ) \
                     ) \
                 ) \
             ) \
         ) \
        $(if $(findstring Cygwin, $(platform)), \
	    $(eval $2_dList = $(strip $(sort $($2_dList)))) \
	    $(eval $2_lList = $(strip $(sort $($2_lList)))) \
	 ) \
        )

$(eval $2_libraryList=$(GEN_LIBLIST))
$(eval $2_libraryListNoshared=$(GEN_LIBLIST_NOSHARED))

$(if $($2_lList), \
	$(if $(findstring Cygwin, $(platform)), \
	  $(eval $2_libraryList += $(sort $($2_lList) $($2_dList)) \
	  $(eval $2_libraryListNoshared += $(NOSHARED_ON) $(NOSHARED_OFF) ) ), \
	  $(eval $2_libraryList += $($2_lList) \
	  $(eval $2_libraryListNoshared += $(NOSHARED_ON) $(NOSHARED_OFF) ) ) \
	) \
)

$(if $(filter /vw,$1),,
$(if $5, \
	$(eval $2_lib_prereq = $(CURDIR)/../lib/lib$2.a) \
	$(eval $2_clean_prereq = clean_lib_static_$2) \
	$(eval $2_install_prereq = install_lib_static_$2), \
	$(if $(findstring Cygwin, $(platform)), \
	     $(eval $2_lib_prereq =  $(if $(MAKE_NOSTATIC),,$(CURDIR)/../lib/lib$2.a) $(CURDIR)/../lib/lib$2.$(SHLIB_EXT) $(CURDIR)/../lib/lib$2.$(SHLIB_EXT).a $(CURDIR)/../lib/lib$2.$(SHLIB_EXT).dep), \
        $(eval $2_lib_prereq =  $(if $(MAKE_NOSTATIC),,$(CURDIR)/../lib/lib$2.a) $(CURDIR)/../lib/lib$2.so ) \
	) \
	$(eval $2_clean_prereq = $(if $(MAKE_NOSTATIC),,clean_lib_static_$2) clean_lib_shared_$2) \
        $(eval $2_install_prereq = $(if $(MAKE_NOSTATIC),,install_lib_static_$2) install_lib_shared_$2 ) \
) \
)

do_lib_$2: $($2_lib_prereq)

clean_lib_$2: $($2_clean_prereq) clean_gcov_lib_$2
	$(AT)$(RM) $($2_depList) $($2_expObjList)
	$(AT) if [ -f $2-restart-da.mk ]; then $(RM) $2-restart-da.mk; fi

.PHONY: clean_gcov_lib_$2
clean_gcov_lib_$2:
	$(AT)$(RM) $(wildcard $(patsubst %.o,%.gc*,$($2_expObjList)))


clean_dist_lib_$2: clean_lib_$2;

install_lib_$2: $($2_install_prereq)


$(CURDIR)/../lib/lib$2.a: $$(xyz_$2_OBJ) 
	@echo "== Making library: ../lib/lib$2.a" 
	-$(AT)$(RM) ../lib/lib$2.a 
	$(AT)$(AR) rc  ../lib/lib$2.a $$(xyz_$2_OBJ)
	$(AT)$(RANLIB) ../lib/lib$2.a


../lib/lib$2.a: $(CURDIR)/../lib/lib$2.a

$(CURDIR)/../lib/lib$2.$(SHLIB_EXT): $$(xyz_$2_OBJ) $$($2_lList) 
	@echo "== Making library: ../lib/lib$2.$(SHLIB_EXT)" 
	-$(AT)$(RM) ../lib/lib$2.$(SHLIB_EXT)
ifeq ($(platform),Cygwin)
	$(AT)$(CXX) $(LDFLAGS_GCOV) -shared -fPIC $$($2_sharedLibName) -Wl,--enable-auto-image-base -Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-runtime-pseudo-reloc $(L_PATH) -Wl,--whole-archive $$(xyz_$2_OBJ) -Wl,--no-whole-archive $(sort $($2_libraryList)) $4 -o ../lib/lib$2.$(SHLIB_EXT)
else
	$(AT)$(CXX) $(LDFLAGS_GCOV) -shared -fPIC $$($2_sharedLibName) -Wl,--copy-dt-needed-entries $(L_PATH) $4 -o ../lib/lib$2.$(SHLIB_EXT) $$(xyz_$2_OBJ) $($2_libraryList) 
endif
	$(AT) if [ "$$$$MAKE_NOSYMBOL_CHECK" == "" ]; then acsMakeCheckUnresolvedSymbols -w ../lib/lib$2.$(SHLIB_EXT); fi 
	$(AT) chmod a-w+x ../lib/lib$2.$(SHLIB_EXT)

../lib/lib$2.$(SHLIB_EXT): $(CURDIR)/../lib/lib$2.$(SHLIB_EXT)

ifeq ($(platform),Cygwin)
$(CURDIR)/../lib/lib$2.$(SHLIB_EXT).a: ../lib/lib$2.$(SHLIB_EXT)
	@echo "== Making import library: ../lib/lib$2.$(SHLIB_EXT).a"
	-$(AT)$(RM) ../lib/lib$2.$(SHLIB_EXT).a
	ln -s ../lib/lib$2.$(SHLIB_EXT) ../lib/lib$2.$(SHLIB_EXT).a

../lib/lib$2.$(SHLIB_EXT).a: $(CURDIR)/../lib/lib$2.$(SHLIB_EXT).a

$(CURDIR)/../lib/lib$2.$(SHLIB_EXT).dep: ../lib/lib$2.$(SHLIB_EXT)
	@echo "== Making library dependencies: ../lib/lib$2.$(SHLIB_EXT).dep"
	-$(AT)$(RM) ../lib/lib$2.$(SHLIB_EXT).dep
	-$(AT)echo "$(strip $(filter-out -lTAO%,$(filter-out -lACE,$(sort $($2_lList) $($2_dList)))))" | sed "s/-l//g"| sed "s/ \+/\n/g" |sort -u | tr '\n' ' ' > ../lib/lib$2.$(SHLIB_EXT).dep

../lib/lib$2.$(SHLIB_EXT).dep: $(CURDIR)/../lib/lib$2.$(SHLIB_EXT).dep
endif


.PHONY: clean_lib_shared_$2
clean_lib_shared_$2: 
	-$(AT)$(RM) ../lib/lib$2.$(SHLIB_EXT) 
ifeq ($(platform),Cygwin)
	-$(AT)$(RM) ../lib/lib$2.$(SHLIB_EXT).a
	-$(AT)$(RM) ../lib/lib$2.$(SHLIB_EXT).dep
endif

.PHONY: clean_lib_static_$2
clean_lib_static_$2: 
	-$(AT)$(RM) ../lib/lib$2.a 

.PHONY: install_lib_static_$2
install_lib_static_$2:$(LIB)/lib$2.a 

$(LIB)/lib$2.a: $(CURDIR)/../lib/lib$2.a
	$(AT)echo "$2"
	$(AT)if [ -f ../lib/lib$2.a ]; then cp ../lib/lib$2.a $(LIB)/lib$2.a;  \
	chmod $(P755) $(LIB)/lib$2.a; else echo "WARNING ../lib/lib$2.a MISSING"; fi 

ifeq ($(platform),Cygwin)
install_lib_shared_$2:$(LIB)/lib$2.$(SHLIB_EXT) $(LIB)/lib$2.$(SHLIB_EXT).a $(LIB)/lib$2.$(SHLIB_EXT).dep
else
install_lib_shared_$2:$(LIB)/lib$2.$(SHLIB_EXT)
endif
$(LIB)/lib$2.$(SHLIB_EXT): $(CURDIR)/../lib/lib$2.$(SHLIB_EXT)
	-$(AT)rm -f $(LIB)/lib$2.$(SHLIB_EXT); 
	$(AT)cp ../lib/lib$2.$(SHLIB_EXT) $(LIB)/lib$2.$(SHLIB_EXT);  
	$(AT)chmod $(P755) $(LIB)/lib$2.$(SHLIB_EXT)
	$(AT)chmod ugo-w+x $(LIB)/lib$2.$(SHLIB_EXT)
ifeq ($(platform),Cygwin)
$(LIB)/lib$2.$(SHLIB_EXT).a: $(CURDIR)/../lib/lib$2.$(SHLIB_EXT).a
	-$(AT)rm -f $(LIB)/lib$2.$(SHLIB_EXT).a;
	$(AT)cp -P ../lib/lib$2.$(SHLIB_EXT).a $(LIB)/lib$2.$(SHLIB_EXT).a;
	$(AT)chmod $(P755) $(LIB)/lib$2.$(SHLIB_EXT).a
	$(AT)chmod ugo-w+x $(LIB)/lib$2.$(SHLIB_EXT).a

$(LIB)/lib$2.$(SHLIB_EXT).dep: $(CURDIR)/../lib/lib$2.$(SHLIB_EXT).dep
	-$(AT)rm -f $(LIB)/lib$2.$(SHLIB_EXT).dep;
	$(AT)cp -P ../lib/lib$2.$(SHLIB_EXT).dep $(LIB)/lib$2.$(SHLIB_EXT).dep;
	$(AT)chmod $(P755) $(LIB)/lib$2.$(SHLIB_EXT).dep
	$(AT)chmod ugo-wx $(LIB)/lib$2.$(SHLIB_EXT).dep
endif

endef


#############################################################
#############################################################
#$(call acsMakeExecutableDependencies,$(VW),$(exe),$($(exe)_OBJECTS),$($(exe)_LDFLAGS),$($(exe)_NOSHARED),$($(exe)_LIBS))

define acsMakeExecutableDependencies

$2_exe_depList =
$2_exe_objList =
$(foreach obj,$3,$(eval $$2_exe_depList += ../object/$(obj).d))
$(foreach obj,$3,$(eval $$2_exe_objList += ../object/$(obj).o))

clean_dependencies_target += $($2_exe_depList)

ifeq ($(DEPENDENCIES),on)
   ifeq ($(MAKE_DEBUG),on)
      include $($2_exe_depList)
   else
      -include $($2_exe_depList)
   endif
endif


$(eval $2_exe_lList = ) 
$(if $(or $3,$6),
    $(eval $2_oList = $(foreach obj,$3,../object/$(obj).o)) \
    $(foreach lib, $6, \
        $(if $(filter stdc++,$(lib)), \
	    $(eval $2_exe_lList += -l$(lib)  MESSAGE += somemessage ), \
            $(if $(filter g++,$(lib)), \
                $(eval MESSAGE += ecgs does not provide), \
                $(if $(filter iostream,$(lib)), \
                    $(eval MESSAGE += please remove iostream),
                    $(if $(filter C++,$(lib)), \
                        $(eval $2_exe_lList += -lstdc++), \
	                $(if $(findstring Cygwin, $(platform)), \
	                    $(eval $2_exe_lList += -l$(lib) $(eval $2_exe_dList += $(call deps,$(lib)))), \
                            $(eval $2_exe_lList += -l$(lib)) \
                         ) \
                     ) \
                 ) \
             ) \
         ) \
     ) \
    $(if $(findstring Cygwin, $(platform)), \
	   $(eval $2_exe_dList = $(strip $(sort $($2_exe_dList)))) \
	   $(eval $2_exe_lList = $(strip $(sort $($2_exe_lList)))) \
     ) \
 )

$(eval $2_libraryList=$(GEN_LIBLIST))
$(eval $2_libraryListNoshared=$(GEN_LIBLIST_NOSHARED))

$(if $($2_exe_lList), \
	$(if $(findstring Cygwin, $(platform)), \
	  $(eval $2_libraryList += $(sort $($2_exe_lList) $($2_exe_dList)) \
	  $(eval $2_libraryListNoshared += $(NOSHARED_ON) $($2_exe_lList) $(NOSHARED_OFF) ) ), \
	  $(eval $2_libraryList += $($2_exe_lList) \
	  $(eval $2_libraryListNoshared += $(NOSHARED_ON) $($2_exe_lList) $(NOSHARED_OFF) ) ) \
	) \
)


.PHONY: $2 
do_exe_$2: ../bin/$2 

../bin/$2: $($2_exe_objList) $(patsubst -lstdc++,,$($2_exe_lList))
	-@echo == Building executable: ../bin/$2 
	-@echo $(MESSAGE)
ifdef MAKE_VXWORKS
	$(AT)$(LD) $(LDFLAGS) $(L_PATH) $4 -r $($2_exe_objList)  $($2_exe_lList)  -o ../bin/$2
else
ifeq ($(strip $(MAKE_NOSHARED) $($2_NOSHARED)),)
ifeq ($(platform),Cygwin)
	$(AT)$(PURIFY) $(PURECOV) $(LD) $(CFLAGS) -Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-runtime-pseudo-reloc -Wl,--whole-archive $($2_exe_objList) -Wl,--no-whole-archive $(LDFLAGS) $(L_PATH) $4 $($2_libraryList)  -o ../bin/$2
else
	$(AT)$(PURIFY) $(PURECOV) $(LD) $(CFLAGS) -Wl,--copy-dt-needed-entries $(LDFLAGS) $(L_PATH) $4 $($2_exe_objList)  $($2_libraryList)  -o ../bin/$2
endif
else
	$(AT)$(PURIFY) $(PURECOV) $(LD) $(CFLAGS) $(LDFLAGS) $(L_PATH) $4  $($2_exe_objList) $($2_libraryListNoshared)  -o ../bin/$2
endif
endif

.PHONY: clean_exe_$2
clean_exe_$2: clean_gcov_exe_$2
	$(AT)$(RM) ../bin/$2 $($2_exe_objList) $($2_exe_depList)

.PHONY: clean_gcov_exe_$2
clean_gcov_exe_$2:
	$(AT)$(RM) $(wildcard $(patsubst %.o,%.gc*,$($2_exe_objList)))

.PHONY: clean_dist_exe_$2
clean_dist_exe_$2: clean_exe_$2;

.PHONY: install_exe_$2
install_exe_$2: $(BIN)/$2

$(BIN)/$2: ../bin/$2
	$(AT)cp ../bin/$2 $(BIN)/$2
	$(AT)chmod $(P755) $(BIN)/$2

endef

#############################################################
#############################################################

# $(call acsMakeLogTSDependencies, $(*F))

define 	acsMakeLogTSDependencies

# preliminary, compute which targets are wanted
ifeq ($(call mustBuild,Java),true)
    ALL_LOGTS += ../lib/$1LTS.jar 
endif
ifeq ($(call mustBuild,C++),true)
ifdef MAKE_VXWORKS
    ALL_LOGTS += ../bin/$1LTS
else
    ALL_LOGTS += $(CURDIR)/../lib/lib$1LTS.a $(CURDIR)/../lib/lib$1LTS.$(SHLIB_EXT)
endif 
endif
ifeq ($(call mustBuild,Python),true) 
    ALL_LOGTS += ../lib/python/site-packages/$1LTS.py 
endif

# MAIN TARGET FOR DO_ALL

.PHONY: check_logts_$1
check_logts_$1:
	$(AT)loggingtsGenCheckXML ../idl/$1.xml

do_logts_$1: check_logts_$1 $$(ALL_LOGTS)

../object/$1.cpp ../object/$1.h:  ../idl/$1.xml
ifeq ($(call mustBuild,C++),true)
	$(AT)echo "== LOGTS generating C++ from ($1) XML " 
	$(AT)loggingtsGenH ../idl/$1.xml ../object/$1.h
	$(AT)loggingtsGenCpp ../idl/$1.xml ../object/$1.cpp

endif


../lib/$1LTS.jar:  ../idl/$1.xml
ifndef MAKE_VXWORKS
ifeq ($(call mustBuild,Java),true)
	$(AT)echo "== LOGTS generating Java from ($1) XML " 
	$(AT)loggingtsGenJava ../idl/$1.xml  || exit -2
	-$(AT)echo "== LOGTS Compiling generated Java classes into Stub jarfile ($1)" 
	$(AT)(cd ../object/$1;  export CLASSPATH="`acsMakeJavaClasspath`$(PATH_SEP)."; $(JAVAC) $(JAVA_EDIRS) `find . -name \*.java | tr '\n' ' '` ) || exit -13 ; \
	    if [ xx$(DEBUG) = xxon ]; \
    then \
		(cd ../object/$1; touch ../../lib/$1LTS.jar; find . \( -name \*.class -o -name \*.java \) -exec jar uf ../../lib/$1LTS.jar  \{\} \; ) ; \
	    else\
		(cd ../object/$1; touch ../../lib/$1LTS.jar; find . -name \*.class -exec jar uf ../../lib/$1LTS.jar  \{\} \; ) ;	\
	    fi;\

endif
endif

../lib/python/site-packages/$1LTS.py:  ../idl/$1.xml
ifndef MAKE_VXWORKS
ifeq ($(call mustBuild,Python),true)
	$(AT)echo "== LOGTS generating Python from ($$(<F)) XML " 
	$(AT)loggingtsGenPython ../idl/$1.xml ../object/$1LTS.py || exit -2
	$(AT)cp ../object/$1LTS.py ../lib/python/site-packages/
endif
endif
#
# CLEAN TARGET
#
.PHONY: clean_logts_$1
ifdef MAKE_VXWORKS
clean_logts_$1: clean_exe_$1LTS
	-$(AT)$(RM)  ../lib/$1LTS.jar ../object/$1 ../object/$1.cpp ../object/$1.o ../object/$1.h  ../object/$1LTS.py  ../lib/python/site-packages/$1LTS.py  ../object/$1.d 
else
clean_logts_$1: clean_lib_$1LTS
	-$(AT)$(RM) ../bin/$1LTS ../object/$1.cpp ../object/$1.o ../object/$1.h ../object/$1.d
endif

.PHONY: clean_dist_logts_$1
clean_dist_logts_$1: clean_logts_$1;


#
# INSTALL TARGET
#
.PHONY: install_logts_$1
ifdef MAKE_VXWORKS
install_logts_$1: install_exe_$1LTS
else
install_logts_$1: $(PRJTOP)/include/$1.h $(LIB)/lib$1LTS.a $(LIB)/lib$1LTS.$(SHLIB_EXT) $(PRJTOP)/idl/$1.xml $(LIB)/python/site-packages/$1LTS.py $(LIB)/$1LTS.jar
endif

$(LIB)/python/site-packages/$1LTS.py: ../idl/$1.xml
	-$(AT)echo "== installing LOGTS Python ($1) " 
	$(AT)cp ../lib/python/site-packages/$1LTS.py $(LIB)/python/site-packages/

$(PRJTOP)/idl/$1.xml: ../idl/$1.xml
	-$(AT)echo "== installing XML file with ACS LOGTS ($1) " 
	$(AT)cp ../idl/$1.xml $(PRJTOP)/idl 

$(PRJTOP)/include/$1.h: ../object/$1.h
	$(AT)cp $$< $(PRJTOP)/include/

$(LIB)/$1LTS.jar: ../lib/$1LTS.jar
	-$(AT)echo "== installing LOGTS Java ($1) " 
	$(AT)cp $$< $(LIB)/

endef

#############################################################
#############################################################

define typesFromDDS 
$(if $(wildcard $1), $(shell egrep "^#pragma DCPS_DATA_TYPE" $1 |  sed 's/.*\:\:\([^\"]*\)\"/\1/' ))

endef

#############################################################
#############################################################

# $(call acsMakeDDSDependencies, $(dds))

define acsMakeDDSDependencies

#USER_IDL := -I$(DDS_ROOT) $(USER_IDL)

$1_targetList = $(foreach type,$($1_typesList),../object/$(type)TypeSupport.idl ../object/$(type)TypeSupportImpl.cpp ../object/$(type)TypeSupportImpl.h)


$1_libList = $(foreach type,$($1_typesList),do_lib_$(type)TypeSupportStubs)

do_dds_$1:$$($1_libList)

$$($1_targetList): ../idl/$1.idl
	$(AT)$(ECHO) "== Generating DDS support for IDL-specified types ($$<)"
	$(AT)(cd ../object && cp ../idl/$1.idl .  && $(DDS_ROOT)/bin/dcps_ts.pl $1.idl && $(RM) $1.idl)

# double loop over exensions and types
#
$(foreach type,$($1_typesList), \
    $(foreach ext,$(IDL_EXTENSIONS),../object/$(type)TypeSupport$(ext)): ../object/$1S.cpp ../object/$(type)TypeSupport.idl Makefile
	$(AT)@echo "== IDL Compiling for DDS TAO (C++): $(type)TypeSupport"
	$(AT) $(TAO_IDL) $(MK_IDL_PATH) $(TAO_MK_IDL_PATH) -o ../object $(TAO_IDLFLAGS) ../object/$(type)TypeSupport.idl
)

#
# CLEAN
#
.PHONY : clean_$1

clean_dds_$1: $(foreach type,$($1_typesList),clean_lib_$(type)TypeSupportStubs) clean_lib_$1Stubs
	$(AT)$(RM)   $(foreach ext, $(IDL_EXTENSIONS),../object/$1$(ext)) $(foreach type,$($1_typesList), $(foreach ext,$(IDL_EXTENSIONS),../object/$(type)TypeSupport$(ext))) $$($1_targetList)
#
# INSTALL
#
.PHONY : install_dds_$1

# check for the header files, MZA
install_dds_$1: $(foreach type,$($1_typesList),install_lib_$(type)TypeSupportStubs) $(foreach type,$($1_typesList),$(foreach ext,$(filter %.h,$(IDL_EXTENSIONS)),$(INCLUDE)/$(type)TypeSupport$(ext)) ) ;

#	$(AT)cp $(foreach type,$($1_typesList),$(foreach ext,$(filter %.h,$(IDL_EXTENSIONS)),../object/$(type)TypeSupport$(ext)) ) $(INCLUDE)

$(foreach type,$($1_typesList),\
  $(foreach ext,$(filter %.h,$(IDL_EXTENSIONS)),\
   $(eval $(INCLUDE)/$(type)TypeSupport$(ext): ../object/$(type)TypeSupport$(ext)
	$(AT)cp ../object/$(type)TypeSupport$(ext) $(INCLUDE)/$(type)TypeSupport$(ext) ) \
) )


endef

#############################################################
#############################################################

#$(call top-level,name,variable,variable-install)

define top-level

.PHONY: do_$1s
.PHONY: install_$1s
.PHONY: clean_$1s
.PHONY: clean_dist_$1s

INSTALL_TARGET += install_$1s
CLEAN_TARGET   += clean_$1s
CLEAN_DIST_TARGET +=  clean_dist_$1s
ALL_TARGET     +=  do_$1s

do_$1s: $(tl-$1-prereq) $(addprefix do_$1_, $2)

do_$1: do_$1s

install_$1s: $1_begin $(addprefix install_$1_, $3)
$1_begin:
	@echo ".....$1:" 

clean_$1s: $(addprefix clean_$1_, $2)
clean_dist_$1s: $(addprefix clean_dist_$1_, $2)


endef

define fake-top-level

.PHONY: do_$1s
.PHONY: install_$1s
.PHONY: clean_$1s
.PHONY: clean_dist_$1s

do_$1s: 
	$(AT)$(ECHO) "This target ($$@) is empty"

install_$1s: 
	$(AT)$(ECHO) "This target ($$@) is empty"

clean_$1s: 
	$(AT)$(ECHO) "This target ($$@) is empty"

clean_dist_$1s: 
	$(AT)$(ECHO) "This target ($$@) is empty"

endef


#############################################################
#############################################################

define acsMakePythonPackageDependencies

.PHONY: do_python_package_$1
do_python_package_$1: ../lib/python/site-packages/$1;

$1_python_source_files:= $(if $1,$(shell find $1  -type f ! -path '*/CVS/*' ! -path '*/.svn/*'))
# ICT-5703: use precedence rules of find properly
$1_python_install_files:= $(if $1,$(shell find  $1 ! -path '*/.svn/*' ! -path '*/CVS/*' -type f \( -name \*.py -o -name \*.def \) | sed -n 's/\(.*\).py$$/\1.py\n\1.pyc/p; /\.py/!p' |sort -t\. -k 2 |tr '\n' ' ' ) )

LIB_SCAPE := $(LIB)/python/site-packages/
# ICT-5703: use precedence rules of find properly
$1_python_install_files_path := $(if $1,$(shell find  $1 ! -path '*/.svn/*' ! -path '*/CVS/*' -type f \( -name \*.py -o -name \*.def \) | sed -n 's/$(1)/$$(LIB_SCAPE)$(1)/p' | sed -n 's/\(.*\).py$$/\1.py\n\1.pyc/p; /\.py/!p' |sort -t\. -k 2 |tr '\n' ' ' ) )

LOCAL_LIB_SCAPE := ../lib/python/site-packages/
# ICT-5703: use precedence rules of find properly
$1_python_install_files_path_req := $(if $1,$(shell find  $1 ! -path '*/.svn/*' ! -path '*/CVS/*' -type f \( -name \*.py -o -name \*.def \) | sed -n 's/$(1)/$$(LOCAL_LIB_SCAPE)$(1)/p' | sort -t\. -k 2 |tr '\n' ' ' ) )

../lib/python/site-packages/$1: OUTPUT=../lib/python/site-packages/$1
../lib/python/site-packages/$1: $$($1_python_source_files) Makefile
	@echo "== Making python package: $$(OUTPUT)" 
	$(AT)mkdir -p $$(OUTPUT)
	$(AT)chmod 755 $$(OUTPUT)
	$(AT)tar cf -  $$(filter-out Makefile, $$? ) | (cd  $$(OUTPUT)/..; tar xf - )
	$(AT)touch ../lib/python/site-packages/$1


.PHONY: clean_python_package_$1
clean_python_package_$1:
	$(AT) $(RM) -fr ../lib/python/site-packages/$1
	$(AT) if [ -f $1-restart-dpps.mk ]; then $(RM) $1-restart-dpps.mk; fi

.PHONY: clean_dist_python_package_$1
clean_dist_python_package_$1: clean_python_package_$1;

.PHONY : install_python_package_$1
install_python_package_$1:
	$(AT)mkdir -p $(LIB)/python/site-packages/$1  
	$(AT)$(ECHO) "\tCompiling python $1" 
	$(AT)cp -pr `find ../lib/python/site-packages/$1/* -maxdepth 0 -type d;find ../lib/python/site-packages/$1/* -maxdepth 0 -type f` $(LIB)/python/site-packages/$1/
	$(AT)cd $(LIB)/python/site-packages && python $(PYTHON_ROOT)/lib/python$(PYTHON_VERS)/compileall.py $(PYTHON_OUTPUT) $(LIB)/python/site-packages/$1
	$(AT)chmod -R $(P755) $(LIB)/python/site-packages/$1


endef

#############################################################
#############################################################
define acsMakePythonModDependencies

.PHONY: do_python_module_$1
do_python_module_$1: ../lib/python/site-packages/$1.py;

../lib/python/site-packages/$1.py: $1.py Makefile
	@echo "== Making python module: ../lib/python/site-packages/$1.py" 
	$(AT)cp $1.py ../lib/python/site-packages/$1.py


.PHONY: install_python_module_$1
install_python_module_$1: $(LIB)/python/site-packages/$1.py;

$(LIB)/python/site-packages/$1.py: ../lib/python/site-packages/$1.py
	$(AT)$(ECHO) "\t$1.py"
	$(AT)cp ../lib/python/site-packages/$1.py $(LIB)/python/site-packages
	$(AT)chmod $(P644) $(LIB)/python/site-packages/$1.py
	$(AT)python $(PYTHON_ROOT)/lib/python$(PYTHON_VERS)/compileall.py $(PYTHON_OUTPUT) ../lib/python/site-packages
	$(AT)cp ../lib/python/site-packages/$1.pyc $(LIB)/python/site-packages
	$(AT)chmod $(P644) $(LIB)/python/site-packages/$1.pyc


.PHONY:clean_python_module_$1
clean_python_module_$1:
	$(AT)$(RM) ../lib/python/site-packages/$1.py

.PHONY: clean_dist_python_module_$1
clean_dist_python_module_$1: clean_python_module_$1;


endef

#############################################################
#############################################################
define acsMakeScriptDependencies

.PHONY: do_script_$1
do_script_$1: ../bin/$1;

../bin/$1: $1 Makefile
	@echo "== Making script: ../bin/$1"
	$(AT)cp $1 ../bin/$1
	$(AT)chmod $(P755) ../bin/$1

.PHONY: install_script_$1
install_script_$1: $(BIN)/$1

$(BIN)/$1: ../bin/$1 
	$(AT)$(ECHO) "\t$1"
	$(AT)cp $$< $(BIN)/$1
	$(AT)chmod $(P755) $(BIN)/$1

.PHONY:clean_script_$1 
clean_script_$1:
	$(AT)$(RM) ../bin/$1

.PHONY:clean_dist_script_$1 
clean_dist_script_$1: clean_script_$1;

endef

#############################################################
#############################################################
define acsMakePythonScriptDependencies

.PHONY: do_python_script_$1
do_python_script_$1: ../bin/$1;


ifeq ($(findstring .py,$1)X,X)
../bin/$1: $1.py Makefile
	@$(ECHO) "== Making Python Script: ../bin/$1"
	$(AT)cp $1.py ../bin/$1
	$(AT)chmod $(P755) ../bin/$1	
else
../bin/$1: $1 Makefile
	@$(ECHO) "== Making Python Script: ../bin/$1"
	$(AT)cp $1 ../bin/$1
	$(AT)chmod $(P755) ../bin/$1
endif

.PHONY: install_python_script_$1
install_python_script_$1: $(BIN)/$1

$(BIN)/$1: ../bin/$1 
	$(AT)$(ECHO) "\t$1"
	$(AT)cp $$< $(BIN)/$1
	$(AT)chmod $(P755) $(BIN)/$1

.PHONY: clean_python_script_$1
clean_python_script_$1:
	$(AT)$(RM) ../bin/$1

.PHONY: clean_dist_python_script_$1
clean_dist_python_script_$1:clean_python_script_$1;

endef

#############################################################
#############################################################
define acsMakeTclScriptDependencies

.PHONY: do_tcl_script_$1 
do_tcl_script_$1: ../bin/$1

../bin/$1: $(addsuffix .tcl, $2) Makefile
	@echo "== Making TCL script: $(@)" 
	$(AT)acsMakeTclScript "$(TCL_CHECKER)" "$(WISH)" "$($1_TCLSH)" "$1" "$($1_OBJECTS)" "$($1_LIBS)"  

.PHONY: install_tcl_script_$1
install_tcl_script_$1: $(BIN)/$1 $(if $(wildcard ../app-defaults/X$1),$(PRJTOP)/app-defaults/X$1,)

$(BIN)/$1: ../bin/$1
	-$(AT)echo "	$1"
	$(AT)cp ../bin/$1 $(BIN)/$1;
	$(AT)chmod $(P755) $(BIN)/$1


$(PRJTOP)/app-defaults/X$1: ../app-defaults/X$1
	-$(AT)cp ../app-defaults/X$1 $(PRJTOP)/app-defaults/X$1
	$(AT)chmod $(P644) $(PRJTOP)/app-defaults/X$1

.PHONY: clean_tcl_script_$1
clean_tcl_script_$1:
	$(AT)$(RM) ../bin/$1

.PHONY: clean_dist_tcl_script_$1
clean_dist_tcl_script_$1: clean_tcl_script_$1;


endef

#############################################################
#############################################################
define acsMakeTclLibDependencies

.PHONY: do_tcl_lib_$1 
do_tcl_lib_$1: ../lib/lib$1.tcl

../lib/lib$1.tcl:  $(addsuffix .tcl,$($1_OBJECTS)) Makefile
	@echo "== Making TCL library: $$@" 
	$(AT)acsMakeTclLib "$(TCL_CHECKER)"  "$1" "$($1_OBJECTS)" 

.PHONY: install_tcl_lib_$1
install_tcl_lib_$1: $(LIB)/lib$1.tcl

$(LIB)/lib$1.tcl: ../lib/lib$1.tcl
	-$(AT)echo "\t$1"
	$(AT)rm -rf $(LIB)/lib$1.tcl
	$(AT)cp -r ../lib/lib$1.tcl $(LIB)/lib$1.tcl
	$(AT)chmod -R $(P755) $(LIB)/lib$1.tcl

.PHONY: clean_tcl_lib_$1
clean_tcl_lib_$1:
	$(AT)$(RM) ../lib/lib$1.tcl

.PHONY: clean_dist_tcl_lib_$1
clean_dist_tcl_lib_$1: clean_tcl_lib_$1;


endef

##########################################################################
##########################################################################
# acsMakeRTAIDependencies

define acsMakeRTAIDependencies

rtai_$1_auxprogs = $(if $(and $(wildcard load$1.cpp),$(wildcard unload$1.cpp)),1,)

.PHONY:
do_rtai_$1: ../rtai/$(kernel_install_subfold)/$1.ko $(if $(and $(wildcard load$1.cpp),$(wildcard unload$1.cpp)),do_exe_load$1 do_exe_unload$1,);


xyz_$1_SRC = $(addsuffix .c,$2)

rtai_$1_components = $(if $(and $(filter 1,$(words $2)),$(filter $1,$(word 1,$2))),,$1-objs := $(addsuffix .o,$2))

# ICT-2680: kernel module must be re-built if Makefile was changed.
# I.e. declare dependency on Makefile and clean-up Kbuild environment (otherwise
# the kernel module components will not be re-compiled, as Kbuild doesn't know 
# about a dependency on Makefile)
#.NOTPARALLEL:../rtai/$(kernel_install_subfold)/$1.ko
CPU := $(shell uname -p)
../rtai/$(kernel_install_subfold)/$1.ko: $$(xyz_$1_SRC) ../bin/installLKM-$1 Makefile
ifeq ($(CPU),x86_64)
	+$(AT)if [ -f Kbuild ]; then $(MAKE) -C $(KDIR) CC=$(CCRTAI) RTAI_CONFIG=$(RTAI_CONFIG) M=$(PWD) clean ; fi
else
	+$(AT)if [ -f Kbuild ]; then $(MAKE) -C $(KDIR) CC=$(CCRTAI) ARCH=i386 RTAI_CONFIG=$(RTAI_CONFIG) M=$(PWD) clean ; fi
endif
	@$(ECHO) "== Making RTAI Module: $1" 
# here we have to generate the hineous Kbuild file
	$(AT)lockfile -s 2 -r 10 Kbuild.lock || echo "WARNING, ignoring lock Kbuild.lock"
	$(AT)$(ECHO) "obj-m += $1.o" > Kbuild
	$(AT)$(ECHO) "$$(rtai_$1_components)" >> Kbuild
	$(AT)$(ECHO) "" >> Kbuild
	$(AT)$(ECHO) "USR_INC := $(USR_INC)"   >> Kbuild
	$(AT)$(ECHO) "EXTRA_CFLAGS := $(EXTRA_CFLAGS)" >> Kbuild
	$(AT)$(ECHO) "KBUILD_EXTRA_SYMBOLS=\"$(RTAI_HOME)/modules/Module.symvers\"" >> Kbuild
ifdef MAKE_VERBOSE
ifeq ($(CPU),x86_64)
	+$(AT)$(MAKE) -C $(KDIR) CC=$(CCRTAI) RTAI_CONFIG=$(RTAI_CONFIG) M=$(PWD) V=2 modules
else
	+$(AT)$(MAKE) -C $(KDIR) CC=$(CCRTAI) ARCH=i386 RTAI_CONFIG=$(RTAI_CONFIG) M=$(PWD) V=2 modules
endif
else
ifeq ($(CPU),x86_64)
	+$(AT)$(MAKE) -C $(KDIR) CC=$(CCRTAI) RTAI_CONFIG=$(RTAI_CONFIG) M=$(PWD) V=0 modules
else
	+$(AT)$(MAKE) -C $(KDIR) CC=$(CCRTAI) ARCH=i386 RTAI_CONFIG=$(RTAI_CONFIG) M=$(PWD) V=0 modules
endif
endif
	$(AT)$(RM) Kbuild.lock
	$(AT)mv $1.ko ../rtai/$(kernel_install_subfold)

# LKM Support binaries
.PHONY: clean_rtai_$1
clean_rtai_$1i:
ifeq ($(CPU),x86_64)
	+$(AT)if [ -f Kbuild ]; then $(MAKE) -C $(KDIR) CC=$(CCRTAI) RTAI_CONFIG=$(RTAI_CONFIG) M=$(PWD) clean ; fi
else
	+$(AT)if [ -f Kbuild ]; then $(MAKE) -C $(KDIR) CC=$(CCRTAI) ARCH=i386 RTAI_CONFIG=$(RTAI_CONFIG) M=$(PWD) clean ; fi
endif
	$(AT)$(RM) ../rtai/$(kernel_install_subfold)/$1.ko $(addprefix ../object/,$(addsuffix .o,$2)) Kbuild.lock ../bin/installLKM-$1

#../bin/installLKM-$1: 
#	@$(ECHO) "echo 'installing $1 into $(PRJTOP)/rtai/$(osrev)..'" > ../bin/installLKM-$1
#	@$(ECHO) "if [ ! -d $(PRJTOP)/rtai/$(osrev) ]; then mkdir $(PRJTOP)/rtai/$(osrev); fi" >> $$@
#	@$(ECHO) "install -d $(PRJTOP)/rtai/$(osrev)" >> $$@
#	@$(ECHO) "install -m 664 -c $(PWD)/../rtai/$(kernel_install_subfold)/$1.ko $(PRJTOP)/rtai/$(osrev)" >> $$@
#ifeq ($$(rtai_$1_auxprogs),1)
#	@$(ECHO) "echo 'setting uid permissions and ownership..'" >> $$@
#	@$(ECHO) "chown root:root $(BIN)/load$1" >> $$@
#	@$(ECHO) "chmod u+s $(BIN)/load$1" >> $$@
#	@$(ECHO) "chown root:root $(BIN)/unload$1" >> $$@
#	@$(ECHO) "chmod u+s $(BIN)/unload$1" >> $$@
#endif
#The following works on an STE
../bin/installLKM-$1:
	@$(ECHO) "echo 'installing $1 into ${PRJTOP}/rtai/${kernel_install_subfold}..'" > ../bin/installLKM-$1
	@$(ECHO) "if [ ! -d $(PRJTOP)/rtai/${kernel_install_subfold} ]; then mkdir $(PRJTOP)/rtai/${kernel_install_subfold}; fi" >> $$@
	@$(ECHO) "install -d $(PRJTOP)/rtai/${kernel_install_subfold}" >> $$@
	@$(ECHO) "install -m 664 -c $(PWD)/../rtai/$(kernel_install_subfold)/$1.ko $(PRJTOP)/rtai/${kernel_install_subfold}" >> $$@
ifeq ($$(rtai_$1_auxprogs),1)
	@$(ECHO) "echo 'setting uid permissions and ownership..'" >> $$@
	@$(ECHO) "chown root:root $(BIN)/load$1" >> $$@
	@$(ECHO) "chmod u+s $(BIN)/load$1" >> $$@
	@$(ECHO) "chown root:root $(BIN)/unload$1" >> $$@
	@$(ECHO) "chmod u+s $(BIN)/unload$1" >> $$@
endif
	@chmod a+x $$@

.PHONY: install_rtai_$1
install_rtai_$1: $(if $(and $(wildcard load$1.cpp),$(wildcard unload$1.cpp)),install_exe_load$1 install_exe_unload$1,) $(PRJTOP)/rtai/$(kernel_install_subfold)/$1.ko 

$(PRJTOP)/rtai/$(kernel_install_subfold)/$1.ko: ../rtai/$(kernel_install_subfold)/$1.ko
	-$(AT)$(ECHO) "\t$1.ko"
	-$(AT)if [ ! -d \$(PRJTOP)/rtai/\$(kernel_install_subfold) ]; then mkdir \$(PRJTOP)/rtai/\$(kernel_install_subfold) ; fi
	$(AT)if [ -f load$1.cpp ]; then \
	    if [ "$(MAKE_RTAI_IGNORE_INSTALL_FAILURE)" != "" ]; then  \
	      if ssh -q -oPasswordAuthentication=no  root@$(HOST) $(PWD)/../bin/installLKM-$1; then \
                echo "Kernel module $1 installed.";  \
              else \
                echo "WARNING: Kernel module $1 not installed"; \
              fi; \
            else  \
             if ssh -q -oPasswordAuthentication=no  root@$(HOST) $(PWD)/../bin/installLKM-$1; then \
                echo "Kernel module $1 installed."; \
             else \
                echo "FAILURE: Kernel module $1 not installed. Check your SSH configuration"; \
                /bin/false;  \
             fi; \
            fi; \
        else \
	$(PWD)/../bin/installLKM-$1; \
        fi 

.PHONY: clean_dist_rtai_$1
clean_dist_rtai_$1:

endef

##########################################################################
##########################################################################
# acsMakeKernelDependencies

define acsMakeKernelDependencies

kernel_module_$1_auxprogs = $(if $(and $(wildcard load$1.cpp),$(wildcard unload$1.cpp)),1,)

.PHONY:
do_kernel_module_$1: ../kernel/$(kernel_install_subfold)/$1.ko $(if $(and $(wildcard load$1.cpp),$(wildcard unload$1.cpp)),do_exe_load$1 do_exe_unload$1,);


xyz_$1_SRC = $(addsuffix .c,$2)

kernel_module_$1_components = $(if $(and $(filter 1,$(words $2)),$(filter $1,$(word 1,$2))),,$1-objs := $(addsuffix .o,$2))

# ICT-2680: kernel module must be re-built if Makefile was changed.
# I.e. declare dependency on Makefile and clean-up Kbuild environment (otherwise
# the kernel module components will not be re-compiled, as Kbuild doesn't know 
# about a dependency on Makefile)
#.NOTPARALLEL:../kernel/$(kernel_install_subfold)/$1.ko
../kernel/$(kernel_install_subfold)/$1.ko: $$(xyz_$1_SRC) ../bin/installLKM-$1 Makefile
	@$(ECHO) "== Making KERNEL Module: $1" 
	+$(AT)if [ -f Kbuild ]; then $(MAKE) -C $(KDIR) CC=$(CCKERNEL) M=$(PWD) clean ; fi
# here we have to generate the hineous Kbuild file
	$(AT)lockfile -s 2 -r 10 Kbuild.lock || echo "WARNING, ignoring lock Kbuild.lock"
	$(AT)$(ECHO) "obj-m += $1.o" > Kbuild
	$(AT)$(ECHO) "$$(kernel_module_$1_components)" >> Kbuild
	$(AT)$(ECHO) "" >> Kbuild
	$(AT)$(ECHO) "USR_INC := $(USR_INC)"   >> Kbuild
	$(AT)$(ECHO) "EXTRA_CFLAGS := $(EXTRA_CFLAGS)" >> Kbuild
	$(AT)$(ECHO) "KBUILD_EXTRA_SYMBOLS=\"$(LINUX_HOME)/modules/Module.symvers\"" >> Kbuild
# ICT-2680: remove "ARCH=i386" from list of compilation flags for plain linux kernel modules
ifdef MAKE_VERBOSE
	+$(AT)$(MAKE) -C $(KDIR) CC=$(CCKERNEL) M=$(PWD) V=2 modules
else
	+$(AT)$(MAKE) -C $(KDIR) CC=$(CCKERNEL) M=$(PWD) V=0 modules
endif
	$(AT)$(RM) Kbuild.lock
	$(AT)mv $1.ko ../kernel/$(kernel_install_subfold)

# LKM Support binaries
.PHONY: clean_kernel_module_$1
# ICT-2680: remove "ARCH=i386" from list of compilation flags for plain linux kernel modules
clean_kernel_module_$1:
	+$(AT)if [ -f Kbuild ]; then $(MAKE) -C $(KDIR) CC=$(CCKERNEL) M=$(PWD) clean ; fi
	$(AT)$(RM) ../kernel/$(kernel_install_subfold)/$1.ko $(addprefix ../object/,$(addsuffix .o,$2)) Kbuild.lock ../bin/installLKM-$1

#The following works on an STE
../bin/installLKM-$1:
	@$(ECHO) "echo 'installing $1 into ${PRJTOP}/kernel/${kernel_install_subfold}..'" > ../bin/installLKM-$1
	@$(ECHO) "if [ ! -d $(PRJTOP)/kernel/${kernel_install_subfold} ]; then mkdir $(PRJTOP)/kernel/${kernel_install_subfold}; fi" >> $$@
	@$(ECHO) "install -d $(PRJTOP)/kernel/${kernel_install_subfold}" >> $$@
	@$(ECHO) "install -m 664 -c $(PWD)/../kernel/$(kernel_install_subfold)/$1.ko $(PRJTOP)/kernel/${kernel_install_subfold}" >> $$@
ifeq ($$(kernel_module_$1_auxprogs),1)
	@$(ECHO) "echo 'setting uid permissions and ownership..'" >> $$@
	@$(ECHO) "chown root:root $(BIN)/load$1" >> $$@
	@$(ECHO) "chmod u+s $(BIN)/load$1" >> $$@
	@$(ECHO) "chown root:root $(BIN)/unload$1" >> $$@
	@$(ECHO) "chmod u+s $(BIN)/unload$1" >> $$@
endif
	@chmod a+x $$@

.PHONY: install_kernel_module_$1
install_kernel_module_$1: $(if $(and $(wildcard load$1.cpp),$(wildcard unload$1.cpp)),install_exe_load$1 install_exe_unload$1,) $(PRJTOP)/kernel/$(kernel_install_subfold)/$1.ko 

$(PRJTOP)/kernel/$(kernel_install_subfold)/$1.ko: ../kernel/$(kernel_install_subfold)/$1.ko
	-$(AT)$(ECHO) "\t$1.ko"
	-$(AT)if [ ! -d \$(PRJTOP)/kernel/\$(kernel_install_subfold) ]; then mkdir \$(PRJTOP)/kernel/\$(kernel_install_subfold) ; fi
	$(AT)if [ -f load$1.cpp ]; then \
	    if [ "$(MAKE_KERNEL_IGNORE_INSTALL_FAILURE)" != "" ]; then  \
	      if ssh -q -oPasswordAuthentication=no  root@$(HOST) $(PWD)/../bin/installLKM-$1; then \
                echo "Kernel module $1 installed.";  \
              else \
                echo "WARNING: Kernel module $1 not installed"; \
              fi; \
            else  \
             if ssh -q -oPasswordAuthentication=no  root@$(HOST) $(PWD)/../bin/installLKM-$1; then \
                echo "Kernel module $1 installed."; \
             else \
                echo "FAILURE: Kernel module $1 not installed. Check your SSH configuration"; \
                /bin/false;  \
             fi; \
            fi; \
        else \
	$(PWD)/../bin/installLKM-$1; \
        fi 

.PHONY: clean_dist_kernel_module_$1
clean_dist_kernel_module_$1:

endef

##########################################################################
##########################################################################

define acsMakeInstallFileDependencies
$(toFile_$1): $1
	$(AT)$(ECHO) "\t$1"
	$(AT)cp $1 $$@
	$(AT)chmod $(P644) $$@


.PHONY:install_file_$1 
install_file_$1: $(toFile_$1)
	$(AT)if [ ! -f $1 ];  then \
	    echo "" >&2 ;\
	    echo " ERROR: acsMakeInstallFiles: " >&2 ;\
	    echo "  >>$1<< file not found " >&2 ;\
	    echo "" >&2 ;\
	    exit 1 ;\
	 fi; 


endef


##########################################################################
##########################################################################


#(info Leaving definitions.mk)




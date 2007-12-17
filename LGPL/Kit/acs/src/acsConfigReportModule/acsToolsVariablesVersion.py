#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) European Southern Observatory, 2002
#
#
# who       when         what
# --------  --------     ----------------------------------------------
# mpasquat  26 SEP 2003  created
#
#************************************************************************
#   NAME                acsToolsVariablesVersion
# 
#   SYNOPSIS            
# 
#   DESCRIPTION         file that contains tools and variable definitions
#                       
#
#   FILES               ACS/LGPL/Kit/acs/src/acsConfigReportModul/acsToolsVariablesVersion.py
#                      
#   ENVIRONMENT
#
#   RETURN VALUES       
#                       
#   CAUTIONS           
#
#   EXAMPLES
#
#   SEE ALSO
#
#   BUGS     
#
#------------------------------------------------------------------------
#

#
# TOOLS
#

#
# Line Structure ('Command Name','Command','Output to be Compared')
#


basic_tools = [('gcc','gcc --version','3.4.6'),\
              ('binutils','ld --version','2.15.92.0.2'),\
              ('make','make --version','3.80'),\
              ('awk','awk --version','3.1.3'),\
              ('gdb','gdb  -version','6.3.0.0'),\
              ('flex','flex --version','2.5.4'),\
              ('bison','bison --version','1.875c'),\
              ('gzip','gzip --version','1.3.3'),\
              ('emacs','emacs -version','21.3.1'),\
              ('sed','sed --version','4.1.2'),\
              ('rcs','rcs -V','5.7'),\
              ('unzip','unzip -v','UnZip 5.51'),\
              ('tar','tar  --version','1.14'),\
              ('expect','expect -version','5.42.1'),\
              ('rman','rman -version','v3.0.8+X.Org'),\
              ('cvs','cvs --version','1.11.17'),\
              ('perl','perl -v','v5.8'),\
              ('zip','zip -h','Zip 2.3'),\
              ('texinfo','info --version','4.7'),\
              ('diffutils','diff --version','2.8.1'),\
              ('groff','true | groff -v','1.18.1.1'),\
              ('java','java -version','(build 1.6.0_02-b05)'),\
              ('Tcl','echo "puts [set ::tcl_patchLevel]; exit" | seqWish','8.4.15'),\
              ('Tk','echo "puts [set ::tcl_patchLevel]; exit" | seqWish','8.4.15'),\
              ('incr Tcl','echo "puts [package require Itcl]; exit" | seqWish','3.4'),\
              ('incr TK','echo "puts [package require Itk]; exit" | seqWish','3.4'),\
              ('iwidgets','echo "puts [package require Iwidgets]; exit" | seqWish','3.0.1'),\
              ('tclX','echo "puts [package require Tclx]; exit" | seqWish','8.4')]

basic_tools_RH9 = [('gcc','gcc --version','3.3'),\
              ('binutils','ld --version','2.14'),\
              ('make','make --version','3.80'),\
              ('awk','awk --version','3.1.3'),\
              ('gdb','gdb  -version','6.0'),\
              ('flex','flex --version','2.5.4'),\
              ('bison','bison --version','1.35'),\
              ('gzip','gzip --version','1.2.4'),\
              ('emacs','emacs -version','21.3.1'),\
              ('sed','sed --version','3.02'),\
              ('rcs','rcs -V','5.7'),\
              ('unzip','unzip -v','UnZip 5.32'),\
              ('tar','tar  --version','1.13'),\
              ('expect','expect -version','5.38.0'),\
              ('rman','rman -version','v3.1'),\
              ('cvs','cvs --version','1.11.5'),\
              ('perl','perl -v','v5.8'),\
              ('zip','zip -h','Zip 2.3'),\
              ('texinfo','info --version','4.5'),\
              ('diffutils','diff --version','2.8.1'),\
              ('groff','true | groff -v','1.17'),\
              ('java','java -version','(build 1.5.0_04-b05)'),\
              ('Tcl','echo "puts [set ::tcl_patchLevel]; exit" | seqWish','8.4.5'),\
              ('Tk','echo "puts [set ::tcl_patchLevel]; exit" | seqWish','8.4.5'),\
              ('incr Tcl','echo "puts [package require Itcl]; exit" | seqWish','3.3'),\
              ('incr TK','echo "puts [package require Itk]; exit" | seqWish','3.3'),\
              ('iwidgets','echo "puts [package require Iwidgets]; exit" | seqWish','4.0.2'),\
              ('tclX','echo "puts [package require Tclx]; exit" | seqWish','8.4'),\
              ('img','echo "puts [package require Img]; exit" | seqWish','1.3'),\
              ('BLT','echo "puts [package require BLT]; exit" | seqWish','2.4'),\
              ('snack','echo "puts [package require snack]; exit" | seqWish','2.2')]


other_tools = [('ACE','more $ACE_ROOT/VERSION','5.6.1'),\
              ('TAO','more $ALMASW_ROOTDIR/$ALMASW_RELEASE/TAO/ACE_wrappers/TAO/VERSION','1.6.1'),\
              ('JacORB','more $JACORB_HOME/doc/REL_NOTES','2.2'),\
              ('Ant','$ANT_HOME/bin/ant -version','1.7.0'),\
              ('doxygen','doxygen --version','1.3.8'),\
              ('python','python -V','2.5.1'),\
              ('mico','more $ALMASW_ROOTDIR/$ALMASW_RELEASE/mico/VERSION','2.3.11'),\
              ('omniorb','cd $ALMASW_ROOTDIR/$ALMASW_RELEASE/Python/omni/; ls THIS_IS_OMNIORB_4_1_0','THIS_IS_OMNIORB_4_1_0')\
               ]


#
# Prepares the dictionary with all set of tools.
#
basic_tools_dic = {}
basic_tools_dic['default'] = basic_tools
basic_tools_dic['Red Hat Enterprise Linux WS release 4 (Nahant Update 1)']  = basic_tools
basic_tools_dic['Red Hat Linux release 9 (Shrike)']    = basic_tools_RH9


#
# VARIABLES
#

#
# Use the keyword --DUMP-- whenever you only want to dump a variable
# without executing any kind of test.
#

acs_variables = [('ACE_ROOT','$ACE_ROOT_DIR/linux'),\
                ('ACE_ROOT_DIR','$ALMASW_ROOTDIR/$ALMASW_RELEASE/TAO/ACE_wrappers/build'),\
                ('ACS_ABEANS_CONFIG','--DUMP--'),\
                ('ACS_CDB','$ACSDATA/config/defaultCDB'),\
                ('ACS_INTERFACE_REPOSITORY','--DUMP--'),\
                ('ACS_LOG_FILE','--DUMP--'),\
                ('ACS_LOG_STDOUT','--DUMP--'),\
                ('ACS_NAME_SERVICE','--DUMP--'),\
                ('ACS_TMP','--DUMP--'),\
                ('ACSDATA','--DUMP--'),\
                ('ACSROOT','$ALMASW_ROOTDIR/$ALMASW_RELEASE/ACSSW'),\
                ('ALMASW_INSTDIR','$ALMASW_ROOTDIR/$ALMASW_RELEASE'),\
                ('ALMASW_RELEASE','--DUMP--'),\
                ('ALMASW_ROOTDIR','--DUMP--'),\
                ('ANT_HOME','$ALMASW_ROOTDIR/$ALMASW_RELEASE/ant'),\
                ('CLASSPATH','$JACORB_HOME/lib/avalon-framework-4.1.5.jar:$JACORB_HOME/lib/logkit-1.2.jar:$JACORB_HOME/lib/jacorb.jar:$JACORB_HOME/lib/idl.jar:$ANT_HOME/lib/ant.jar:'),\
                ('GNU_ROOT','/usr'),\
                ('INTROOT','--DUMP--'),\
                ('JACORB_HOME','$ALMASW_ROOTDIR/$ALMASW_RELEASE/JacORB'),\
                ('JAVA_ENDORSED','--DUMP--'),\
                ('JAVA_HOME','/usr/java/jdk1.6.0_02'),\
                ('JAVA_ORB','--DUMP--'),\
                ('MANAGER_REFERENCE','--DUMP--' ),\
                ('NAMESERVICE_REFERENCE','--DUMP--'),\
                ('MICO_HOME','$ALMASW_INSTDIR/mico'),\
                ('OMNI_ROOT','$ALMASW_ROOTDIR/$ALMASW_RELEASE/Python/omni'),\
                ('OMNIORB_CONFIG','$OMNI_ROOT/config'),\
                ('PYTHON_ROOT','$ALMASW_ROOTDIR/$ALMASW_RELEASE/Python'),\
                ('TCLTK_ROOT','/alma/ACS-7.0/tcltk'),\
                ('INTROOT','--DUMP--'),\
                ('INTLIST','--DUMP--'),\
                ('ACSROOT','--DUMP--'),\
                ('VLTROOT','--DUMP--')]
              
#
acs_variables_RH9= [('ACE_ROOT','$ACE_ROOT_DIR/linux'),\
                ('ACE_ROOT_DIR','$ALMASW_ROOTDIR/$ALMASW_RELEASE/TAO/ACE_wrappers/build'),\
                ('ACS_ABEANS_CONFIG','--DUMP--'),\
                ('ACS_CDB','$ACSDATA/config/defaultCDB'),\
                ('ACS_INTERFACE_REPOSITORY','--DUMP--'),\
                ('ACS_LOG_FILE','--DUMP--'),\
                ('ACS_LOG_STDOUT','--DUMP--'),\
                ('ACS_NAME_SERVICE','--DUMP--'),\
                ('ACS_TMP','--DUMP--'),\
                ('ACSDATA','--DUMP--'),\
                ('ACSROOT','$ALMASW_ROOTDIR/$ALMASW_RELEASE/ACSSW'),\
                ('ALMASW_INSTDIR','$ALMASW_ROOTDIR/$ALMASW_RELEASE'),\
                ('ALMASW_RELEASE','--DUMP--'),\
                ('ALMASW_ROOTDIR','--DUMP--'),\
                ('ANT_HOME','$ALMASW_ROOTDIR/$ALMASW_RELEASE/ant'),\
                ('CLASSPATH','$JACORB_HOME/lib/jacorb.jar:$JACORB_HOME/lib/idl.jar:$ANT_HOME/lib/ant.jar:'),\
                ('GNU_ROOT','$ALMASW_ROOTDIR/$ALMASW_RELEASE/gnu'),\
                ('INTROOT','--DUMP--'),\
                ('JACORB_HOME','$ALMASW_ROOTDIR/$ALMASW_RELEASE/JacORB'),\
                ('JAVA_ENDORSED','--DUMP--'),\
                ('JAVA_HOME','$ALMASW_ROOTDIR/$ALMASW_RELEASE/java/jdk1.5.0_04'),\
                ('JAVA_ORB','--DUMP--'),\
                ('MANAGER_REFERENCE','--DUMP--' ),\
                ('NAMESERVICE_REFERENCE','--DUMP--'),\
                ('MICO_HOME','$ALMASW_INSTDIR/mico'),\
                ('OMNI_ROOT','$ALMASW_ROOTDIR/$ALMASW_RELEASE/Python/omni'),\
                ('OMNIORB_CONFIG','$OMNI_ROOT/config'),\
                ('PYTHON_ROOT','$ALMASW_ROOTDIR/$ALMASW_RELEASE/Python'),\
                ('TCLTK_ROOT','$ALMASW_ROOTDIR/$ALMASW_RELEASE/tcltk'),\
                ('INTROOT','--DUMP--'),\
                ('INTLIST','--DUMP--'),\
                ('ACSROOT','--DUMP--'),\
                ('VLTROOT','--DUMP--')]
              
#
# Prepares the dictionary with all set variables
#
acs_variables_dic = {}
acs_variables_dic['default'] = acs_variables
acs_variables_dic['Red Hat Enterprise Linux WS release 4 (Nahant Update 1)']  = acs_variables
acs_variables_dic['Red Hat Linux release 9 (Shrike)']    = acs_variables_RH9


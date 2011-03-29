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


basic_tools = [('gcc','gcc --version','4.1.2'),\
              ('binutils','ld --version','2.17.50.0.6-14.el5'),\
              ('make','make --version','3.81'),\
              ('awk','awk --version','3.1.5'),\
              ('gdb','gdb --version','7.2-16.el5'),\
              ('flex','flex --version','2.5.4'),\
              ('bison','bison --version','2.3'),\
              ('gzip','gzip --version','1.3.5'),\
              ('emacs','emacs -version','21.4.1'),\
              ('sed','sed --version','4.1.5'),\
              ('rcs','rcs -V','5.7'),\
              ('unzip','unzip -v','UnZip 5.52'),\
              ('tar','tar  --version','1.15.1'),\
              ('expect','expect -version','expect version 5.43.0'),\
              ('rman','rman -version | awk "{ print \$1, \$2 }"','PolyglotMan v3.1'),\
              ('cvs','cvs --version','1.11.22'),\
              ('perl','perl -v','v5.8'),\
              ('zip','zip -h','Zip 2.3'),\
              ('texinfo','info --version','4.8'),\
              ('diffutils','diff --version','2.8.1'),\
              ('groff','true | groff -v','1.18.1.1'),\
              ('java','java -version','(build 1.6.0_20'),\
              ('Tcl','echo "puts [set ::tcl_patchLevel]; exit" | tclsh','8.4.19'),\
              ('Tk','echo "package require Tk; puts [set ::tk_patchLevel]; exit" | tclsh','8.4.19'),\
              ('incr Tcl','echo "puts [package require Itcl]; exit" | tclsh','3.4'),\
              ('incr TK','echo "puts [package require Itk]; exit" | tclsh','3.4'),\
              ('iwidgets','echo "puts [package require Iwidgets]; exit" | tclsh','4.0.2'),\
              ('tclX','echo "puts [package require Tclx]; exit" | tclsh','8.4'),\
              ('img','echo "puts [package require Img]; exit" | tclsh','1.3'),\
              ('BLT','echo "puts [package require BLT]; exit" | tclsh','2.4'),\
              ('Tktable','echo "puts [package require Tktable]; exit" | tclsh','2.9'),\
              ('snack','echo "puts [package require snack]; exit" | tclsh','2.2'),\
              ('expect','echo "puts [package require Expect]; exit" | tclsh','5.43.0')]

other_tools = [('ACE','more $ACE_ROOT/VERSION','5.8.1'),\
              ('TAO','more $ALMASW_ROOTDIR/$ALMASW_RELEASE/TAO/ACE_wrappers/TAO/VERSION','1.8.1'),\
              ('JacORB','more $JACORB_HOME/doc/REL_NOTES','2.2'),\
              ('Ant','$ANT_HOME/bin/ant -version','1.7.1'),\
              ('doxygen','doxygen --version','1.7.0'),\
              ('python','python -V','2.6.5'),\
              ('mico','cat $ALMASW_ROOTDIR/$ALMASW_RELEASE/mico/include/mico/version.h | grep MICO_VERSION | cut -d\'"\' -f2','2.3.13'),\
              ('omniorb','cd $ALMASW_ROOTDIR/$ALMASW_RELEASE/Python/omni/; ls THIS_IS_OMNIORB_4_1_0','THIS_IS_OMNIORB_4_1_0')\
               ]


#
# Prepares the dictionary with all set of tools.
#
basic_tools_dic = {}
basic_tools_dic['default'] = basic_tools
basic_tools_dic['Red Hat Enterprise Linux Server release 5.3 (Tikanga)']  = basic_tools


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
                ('CLASSPATH','$JACORB_HOME/lib/jacorb.jar:$JACORB_HOME/lib/idl.jar:$ANT_HOME/lib/ant.jar'),\
                ('GNU_ROOT','/usr'),\
                ('INTROOT','--DUMP--'),\
                ('JACORB_HOME','$ALMASW_ROOTDIR/$ALMASW_RELEASE/JacORB'),\
                ('JAVA_ENDORSED','--DUMP--'),\
                ('JAVA_HOME','/usr/java/default'),\
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
acs_variables_dic['Red Hat Enterprise Linux Server release 5.3 (Tikanga)']  = acs_variables

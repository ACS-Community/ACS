#! /usr/bin/env python

#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) European Southern Observatory, 2002
#
#
# who       when         what
# --------  --------     ----------------------------------------------
# mpasquat  26 SEP 2003  created
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

#************************************************************************
#   NAME
#   acsConfigReport - Generates a report with the ACS ocnfiguration
# 
#   SYNOPSIS
#   acsConfigReport [-g] [-h] [--norpm]
# 
#   DESCRIPTION
#   this script compares tools version and variables setting
#   to the ones provided in the file "acsToolsVariablesVersion".
#   It also, in Red Hat, compares the rpm version and release with
#   the ones provided in the file "acsPackageInfo-RH-[rel num].rpmref"
#   A report of the test is locally stored in the file "report.html"
#
#   FILES
#   ACS/LGPL/Kit/acs/src/acsConfigReportModule/acsToolsVariablesVersion.py
#                       
#   ENVIRONMENT
#
#   RETURN VALUES       
#                       
#   CAUTIONS
#   When running this script on an unsupported Linux distribution
#   (Red Hat or Scientific Linux are accepted)
#   a warning is issued and a refault RPM file is used.
#   But no warranty can be given on anything to work!!!
#
#   EXAMPLES
#
#   SEE ALSO
#
#   BUGS     
#
#------------------------------------------------------------------------
#

import os
import sys
import getopt
import commands
import acsConfigReportModule.acsToolsVariablesVersion
import string
import platform
import logging

from os.path import join

###################################
# Command line argument parsing
#

def usage():
    print "Generates a report of the ACS/OF configuration"
    print "compared with a reference machine"
    print "The report is saved in the file: report.html"
    print "Options:"
    print "   -h, --help : this help"
    print "   -g         : generetes new reference for report"
    print "   --norpm    : do not compare lists of installed rpms" 

argv = sys.argv[1:] # Strip program name

try:
    opts, rest = getopt.getopt(argv, "gh", ["help", "norpm"])
except getopt.error, err:
    print >>sys.stderr, err.msg
    print >>sys.stderr, "for help use --help"
    sys.exit(2)

generate = False
rpmcheck = True
for option, argument in opts:
    if option == "-g":
        generate = True
    if option in ("--norpm"):
        rpmcheck = False
    if option in ("-h", "--help"):
        usage()
        sys.exit()
    # ...

#################################################################
# Retrieval of O.S. name,Version, Kernel Version or Patch Level
# Now default is RH Enterprise Workstation.
#
rpmDefaultFilename='acsPackageInfo-RH-WS.rpmref'
toolsDefaultKey='default'

tmp_os=commands.getoutput('uname -s')

if tmp_os == 'Linux':
    # Linux
    command='cat /etc/redhat-release'
    command2='uname -r'

release=commands.getoutput(command)

# Retrieve the RH release number
distroName, distroVersion, distroRelease = platform.dist()
releaseNum=distroName + '-' + str(distroVersion)


# We accept both Red Hat and Scientific Linux
standardRedHat=release.count("Red Hat") or release.count("Scientific Linux")

KernelV_PatchL=commands.getoutput(command2)

########################################
# Retrieves ACS Release and Tag
#
acs_version=commands.getoutput('cat $ACSROOT/ACS_VERSION')+'.'+commands.getoutput('cat $ACSROOT/ACS_PATCH_LEVEL')
acs_tag=commands.getoutput('cat $ACSROOT/ACS_TAG')

####################################
# Generation of reference RPM list,
# if selected on command line
#       

acsRoot = os.getenv('ACSROOT')
rpmDirPath = join(acsRoot, 'config') if acsRoot else join(os.path.pardir, 'config')

if generate == True:
    if standardRedHat == False:
        print 'No Red Hat or Scientific Linux distribution!!!'
    else:
        rpmFilename = 'acsPackageInfo-%s.%s' %(releaseNum, 'rpmref')
        rpmrefPath = join(rpmDirPath, rpmFilename)
        rpmQuery='rpm -qa --queryformat "%{NAME} %{VERSION} %{RELEASE}\n"'
        fullCommand = '%s | sort > %s' %(rpmQuery, rpmFilename)
        output=commands.getoutput(fullCommand)
        print ' '
        print 'File %s created in %s' %(rpmFilename, rpmDirPath)
        print ' '
        sys.exit(1)

#####################################################################################
# RPM File checking before proceeding with analisys
# Sequence followed: ../config, $INTROOT/config, $INTLIST[0]/config, $ACSROOT/config
#

# Flag setting
goOn='False'

#
# First checks for a version specific file
#

# Local ../config directory
if standardRedHat == False:
    rpmFilename=rpmDefaultFilename
    print 'No Red Hat distribution!!! Using default file:' + rpmFilename
else:
    rpmFilename = 'acsPackageInfo-' + releaseNum + '.rpmref'

rpmrefPath = join(rpmDirPath, rpmFilename)
fileExistence=os.access(rpmrefPath,os.F_OK)

if fileExistence == False:
    # $INTROOT/config directory
    variableExistence=os.getenv('INTROOT','NOT DEFINED')
    if  variableExistence != 'NOT DEFINED':
        rpmrefPath=os.getenv('INTROOT')+'/config/' + rpmFilename
        fileExistence=os.access(rpmrefPath,os.F_OK)

if fileExistence == False:
    # $INTLIST/config directory
    variableExistence=os.getenv('INTLIST','NOT DEFINED')
    if  variableExistence != 'NOT DEFINED':
        # Take the first path $INTLIST
        intlistPaths=string.split(variableExistence)
        intlistPath=intlistPaths[0]
 
        rpmrefPath=join(intlistPath, 'config', rpmFilename)
        fileExistence=os.access(rpmrefPath,os.F_OK)

if fileExistence == False:
    # $ACSROOT/config directory
    variableExistence=os.getenv('ACSROOT','NOT DEFINED')
    if  variableExistence != 'NOT DEFINED':
        rpmrefPath = join(rpmDirPath, rpmFilename)
        fileExistence=os.access(rpmrefPath,os.F_OK)

#
# If not found, searches again for the default file
# GCH: This is code duplication and the search algorithm
#      should go in a function.
#      More over, the INLIST is not really searched.
#
if fileExistence == False:
    rpmFilename=rpmDefaultFilename
    print 'No Red Hat specific file found!!! Searching for default file:' + rpmFilename
    rpmrefPath = join(rpmDirPath, rpmFilename)
    fileExistence=os.access(rpmrefPath,os.F_OK)

if fileExistence == False:
    # $INTROOT/config directory
    variableExistence=os.getenv('INTROOT','NOT DEFINED')
    if  variableExistence != 'NOT DEFINED':
        rpmrefPath=join(os.getenv('INTROOT'), 'config', rpmFilename)
        fileExistence=os.access(rpmrefPath,os.F_OK)

if fileExistence == False:
    # $INTLIST/config directory
    variableExistence=os.getenv('INTLIST','NOT DEFINED')
    if  variableExistence != 'NOT DEFINED':
        # Take the first path $INTLIST
        intlistPaths=string.split(variableExistence)
        intlistPath=intlistPaths[0]

        rpmrefPath = join(intlistPath, 'config', rpmFilename)
        fileExistence=os.access(rpmrefPath,os.F_OK)

if fileExistence == False:
    # $ACSROOT/config directory
    variableExistence=os.getenv('ACSROOT','NOT DEFINED')
    if  variableExistence != 'NOT DEFINED':
        rpmrefPath = join(os.getenv('ACSROOT'), 'config', rpmFilename)
        fileExistence=os.access(rpmrefPath,os.F_OK)

# If goOn is still equal to 'True' it means that the file was not found

if goOn == 'True':
    # ERROR, file acsPackageInfo-RH-[rel num].rpmref is missing!
    print ' '
    print '*** Error: file ' + rpmFilename + ' is not present in:'
    print ' '
    print '           -) ../config'
    print '           -) $INTROOT/config'
    print '           -) $INTLIST[0]/config'
    print '           -) $ACSROOT/config'
    print ' '
    print '           Please create it using the following command:'
    print ' '
    print '           acsConfigReport -g'
    print ' '
    print '           and move it under ../config.'
    print ' '
    sys.exit(1)
else:
    print ' '
    print ' RPM reference file : '+rpmrefPath
    print ' '

##########################################
# Creates report file
#

# Removal of local html report file
commands.getoutput('rm -f ./report.html')

# Report file Creation
commands.getoutput('echo "<html>" >> ./report.html')
commands.getoutput('echo "<head><title> ACS Configuration Report </title></head>" >> ./report.html')
commands.getoutput('echo "<body bgcolor=#FFFFFF>"  >> ./report.html')
commands.getoutput('echo "<font face="courier new">"  >> ./report.html')

commands.getoutput('echo "<h1>************************************<br>" >> ./report.html')
commands.getoutput('echo "***** ACS Configuration Report *****<br>" >> ./report.html')
commands.getoutput('echo "************************************</h1>" >> ./report.html')
commands.getoutput('echo " <br>" >> ./report.html')
commands.getoutput('echo "Hostname: '+commands.getoutput('hostname')+'<br>" >>  ./report.html')
commands.getoutput('echo "User: '+commands.getoutput('echo $USER')+'<br>" >>  ./report.html')
commands.getoutput('echo "Shell: '+commands.getoutput('echo $SHELL')+'<br>" >>  ./report.html')
commands.getoutput('echo "Time: '+commands.getoutput('export TZ=UTC; date')+'<br>" >>  ./report.html')
commands.getoutput('echo " <br>" >> ./report.html')

commands.getoutput('echo "<h1>****************************************<br>" >> ./report.html')
commands.getoutput('echo "***** ACS Release *****<br>" >> ./report.html')
commands.getoutput('echo "****************************************</h1>" >> ./report.html')
commands.getoutput('echo " <br>" >> ./report.html')
commands.getoutput('echo "ACS_VERSION: '+acs_version+'<br>" >>  ./report.html')
commands.getoutput('echo "ACS_TAG: '+acs_tag+'<br>" >>  ./report.html')

commands.getoutput('echo "<h1>****************************************<br>" >> ./report.html')
commands.getoutput('echo "***** Operating System and Release *****<br>" >> ./report.html')
commands.getoutput('echo "****************************************</h1>" >> ./report.html')
commands.getoutput('echo " <br>" >> ./report.html')
commands.getoutput('echo "O.S.:'+tmp_os+'<br>" >>  ./report.html')
commands.getoutput('echo "'+release+'<br>" >>  ./report.html')

if tmp_os == 'SunOS':
    commands.getoutput('echo "Patch Level:'+KernelV_PatchL+'<br>" >>  ./report.html')
if tmp_os == 'Linux':
    commands.getoutput('echo "Kernel Version:'+KernelV_PatchL+'<br>" >>  ./report.html')

commands.getoutput('echo " <br>" >> ./report.html')
commands.getoutput('echo "<h1>***********************<br>" >> ./report.html')
commands.getoutput('echo "***** Basic Tools *****<br>" >> ./report.html')
commands.getoutput('echo "***********************</h1>" >> ./report.html')
commands.getoutput('echo " <br>" >> ./report.html')

tmp_counter=0

if acsConfigReportModule.acsToolsVariablesVersion.basic_tools_dic.has_key(release):
    basic_tools=acsConfigReportModule.acsToolsVariablesVersion.basic_tools_dic[release]
else:   
    basic_tools=acsConfigReportModule.acsToolsVariablesVersion.basic_tools_dic[toolsDefaultKey]
    commands.getoutput('echo " Unsupported release. Using default toolset configuration<br>" >> ./report.html')

for x in range(0, len(basic_tools)):
    tmp_counter=tmp_counter+1
    name,command,exout=basic_tools[x]
    result=commands.getoutput(command)
    result=string.replace(result,"`","")
    result=string.replace(result,"'","")

    if result.count(exout) == 0:
        commands.getoutput(
                'echo "' +
                str(tmp_counter) +
                ') ' + 
                name +
                ': Version = ' +
                result +
                '<br>" >>  ./report.html')
        commands.getoutput(
                'echo "<font color=#ff0000>===> Expected Version : ' +
                exout +
                '</font><br>"  >>  ./report.html')
        commands.getoutput('echo " <br>" >> ./report.html')
    else:
        commands.getoutput('echo "' +
                str(tmp_counter) +
                ') ' + 
                name +
                ': Version = ' + 
                exout +
                '<br>" >>  ./report.html')
        commands.getoutput('echo " <br>" >> ./report.html')
     
commands.getoutput('echo " <br>" >> ./report.html')  

commands.getoutput('echo "<h1>************************<br>" >> ./report.html')
commands.getoutput('echo "***** Others Tools *****<br>" >> ./report.html')
commands.getoutput('echo "************************</h1>" >> ./report.html')
commands.getoutput('echo " <br>" >> ./report.html')

tmp_counter=0
for x in range(0, len(acsConfigReportModule.acsToolsVariablesVersion.other_tools)):
    tmp_counter=tmp_counter+1
    name,command,exout=acsConfigReportModule.acsToolsVariablesVersion.other_tools[x]
    result=commands.getoutput(command)
    result=string.replace(result,"`","")
    result=string.replace(result,"'","")

    if result.count(exout) == 0:
        commands.getoutput(
                'echo "' + 
                str(tmp_counter) +
                ' ) ' +
                name +
                ': Version = ' +
                result +
                '<br>" >>  ./report.html')
        commands.getoutput(
                'echo "<font color=#ff0000>===> Expected Version : ' +
                exout+'</font><br>"  >>  ./report.html')
        commands.getoutput('echo " <br>" >> ./report.html')
    else:
        commands.getoutput('echo "' +
                str(tmp_counter) +
                ') ' +
                name +
                ': Version = ' +
                exout +
                '<br>" >>  ./report.html')
        commands.getoutput('echo " <br>" >> ./report.html')
 
commands.getoutput('echo " <br>" >> ./report.html')
      
commands.getoutput('echo " " >> ./report.html')
commands.getoutput('echo "<h1>*************************************<br>" >> ./report.html')
commands.getoutput('echo "***** ACS Environment Variables *****<br>" >> ./report.html')
commands.getoutput('echo "*************************************</h1>" >> ./report.html')
commands.getoutput('echo " <br>" >> ./report.html')

#
# For Linux
#

my_string='===> Expected Value'

if acsConfigReportModule.acsToolsVariablesVersion.acs_variables_dic.has_key(release):
    acs_variables=acsConfigReportModule.acsToolsVariablesVersion.acs_variables_dic[release]
else:   
    acs_variables=acsConfigReportModule.acsToolsVariablesVersion.acs_variables_dic[toolsDefaultKey]
    commands.getoutput('echo " Unsupported release. Using default toolset configuration<br>" >> ./report.html')

for x in range(0, len(acs_variables)):    
    commands.getoutput('echo " <br>" >> ./report.html')
    command,argument=acs_variables[x]
    result_command=commands.getoutput('echo $'+command)
    commands.getoutput('echo "'+command.ljust(23)+' = '+result_command+'<br>" >>  ./report.html')
    if argument != '--DUMP--':
        result_default=commands.getoutput('echo '+argument)
        if result_command != result_default:
            commands.getoutput(
                    'echo "<font color=#ff0000>' +
                    my_string.ljust(23) +
                    ' = ' +
                    result_default +
                    '</font><br>" >>  ./report.html') 
        else:
            # Variable and value are equal. Investigation on ACSROOT
            if command=='ACSROOT':
                num_alma=result_command.count("/alma/",0,6)
                if num_alma!=1:
                    commands.getoutput(
                            'echo "<font color=#ff0000>' +
                            my_string.ljust(23) +
                            ' = ' +
                            result_default +
                            '</font><br>" >>  ./report.html')


########################################################################     
# SPECIAL VARIABLES. 
#
# The type of test here performed is inherited from .bash_profile.acs
########################################################################

#!#tmp_vltroot=commands.getoutput('echo $VLTROOT')
tmp_acsroot=commands.getoutput('echo $ACSROOT')

tmp_intlist_oneword=commands.getoutput('echo $INTLIST')
tmp_intlist=tmp_intlist_oneword.split(":")
tmp_intlistPath=tmp_intlist[0]

tmp_introot=commands.getoutput('echo $INTROOT')
tmp_modpath=commands.getoutput('echo $MODPATH')
tmp_variableExistence=os.getenv('INTLIST','NOT DEFINED')

tmp_windbase=os.getenv('WIND_BASE','NOT_DEFINED')

#################
# IDL_PATH
#

commands.getoutput('echo " <br>" >> ./report.html')
tmp_idl_path='-I$ACE_ROOT/TAO/orbsvcs/orbsvcs -I$ACE_ROOT/TAO/orbsvcs -I$ACE_ROOT/TAO -I$ACE_ROOT/TAO/tao'

#!#if ((tmp_vltroot != '') and (tmp_vltroot != tmp_intlistPath)):
#!#   tmp_idl_path='-I$VLTROOT/idl '+tmp_idl_path	

#!#if ((tmp_acsroot != '') and (tmp_vltroot != tmp_acsroot)):
if ((tmp_acsroot != '') and (tmp_acsroot != tmp_intlistPath)):
   tmp_idl_path='-I$ACSROOT/idl '+tmp_idl_path

if tmp_intlist[0] != '':
    tmp_list=''
    for item_list in tmp_intlist:
        tmp_list=tmp_list+'-I'+item_list+'/idl '

    if tmp_introot != '':
        tmp_idl_path='-I'+tmp_introot+'/idl '+tmp_list+' '+tmp_idl_path
    else:
        tmp_idl_path=tmp_list+' '+tmp_idl_path
else:
    if tmp_introot != '':
        tmp_idl_path='-I'+tmp_introot+'/idl '+tmp_idl_path

if tmp_modpath == '1':
    tmp_idl_path='-I../idl '+tmp_idl_path  

result_command=commands.getoutput('echo $IDL_PATH')
tmp_idl_path=commands.getoutput('echo '+tmp_idl_path+'')
command='IDL_PATH'
commands.getoutput('echo "'+command.ljust(23)+' = '+result_command+'<br>" >>  ./report.html')

if tmp_idl_path != result_command:
    commands.getoutput('echo "<font color=#ff0000>'+my_string.ljust(23)+' = '+tmp_idl_path+'</font><br>" >>  ./report.html')

#########################
# PYTHONPATH
#
     
commands.getoutput('echo " <br>" >> ./report.html')
if tmp_intlist[0] != '':
    tmp_list=' '
    for item_list in tmp_intlist:
        tmp_list=tmp_list+item_list+'/lib/python/site-packages:'

    if tmp_introot != '':
        if tmp_acsroot == '':
            tmp_PYTHONPATH='$INTROOT/lib/python/site-packages:'+tmp_list+'$OMNI_ROOT/lib/python:$OMNI_ROOT/lib:$PYTHON_ROOT/lib/python2.6/site-packages:$OMNI_ROOT/lib/python/site-packages'
        else:
            tmp_PYTHONPATH='$INTROOT/lib/python/site-packages:'+tmp_list+'$ACSROOT/lib/python/site-packages:$OMNI_ROOT/lib/python:$OMNI_ROOT/lib:$PYTHON_ROOT/lib/python2.6/site-packages:$OMNI_ROOT/lib/python/site-packages'
    else:
        if tmp_acsroot == '':
            tmp_PYTHONPATH=tmp_list+'$OMNI_ROOT/lib/python:$OMNI_ROOT/lib:$PYTHON_ROOT/lib/python2.6/site-packages:$OMNI_ROOT/lib/python/site-packages'
        else:
            tmp_PYTHONPATH=tmp_list+'$ACSROOT/lib/python/site-packages:$OMNI_ROOT/lib/python:$OMNI_ROOT/lib:$PYTHON_ROOT/lib/python2.6/site-packages:$OMNI_ROOT/lib/python/site-packages'    
else:
    if tmp_introot != '':
        # ACSROOT must exist here since INTLIST is nott defined
        tmp_PYTHONPATH='$INTROOT/lib/python/site-packages:$ACSROOT/lib/python/site-packages:$OMNI_ROOT/lib/python:$OMNI_ROOT/lib:$PYTHON_ROOT/lib/python2.6/site-packages:$OMNI_ROOT/lib/python/site-packages'
    else:
        tmp_PYTHONPATH='$ACSROOT/lib/python/site-packages:$OMNI_ROOT/lib/python:$OMNI_ROOT/lib:$PYTHON_ROOT/lib/python2.6/site-packages:$OMNI_ROOT/lib/python/site-packages'
   
if tmp_modpath == '1':
    tmp_PYTHONPATH='../lib/python/site-packages:'+tmp_PYTHONPATH

result_command=commands.getoutput('echo $PYTHONPATH')
tmp_PYTHONPATH=commands.getoutput('echo '+tmp_PYTHONPATH+'')
command='PYTHONPATH'
commands.getoutput('echo "'+command.ljust(23)+' = '+result_command+'<br>" >>  ./report.html')

if tmp_PYTHONPATH != result_command:
    commands.getoutput('echo "<font color=#ff0000>'+my_string.ljust(23)+' = '+tmp_PYTHONPATH+'</font><br>" >>  ./report.html')

###################3
# PATH
#

commands.getoutput('echo " <br>" >> ./report.html')
if tmp_os == 'Linux':
    # So it is linux
    tmp_PATH='$JAVA_HOME/bin:$ANT_HOME/bin:$JACORB_HOME/bin:$PYTHON_ROOT/bin:$OMNI_ROOT/bin:$TCLTK_ROOT/bin:$GNU_ROOT/bin:$ORIG_PATH'

#!#   if ((tmp_vltroot != '') and (tmp_vltroot !=  tmp_intlistPath)): 
#!#      tmp_PATH='$VLTROOT/bin:'+tmp_PATH

#!#   if ((tmp_acsroot != '') and (tmp_vltroot != tmp_acsroot)):
    if ((tmp_acsroot != '') and (tmp_acsroot != tmp_intlistPath)):
        tmp_PATH='$ACSROOT/bin:'+tmp_PATH
else:
    tmp_PATH='IMPOSSIBLE_TO_DETERMINE_THE_SYSTEM_NAME_OR_OS_NOT_SUPPORTED'

if tmp_intlist[0] != '':
    tmp_list=''
    for item_list in tmp_intlist:
        tmp_list=tmp_list+item_list+'/bin:'

    if tmp_introot != '':
        tmp_PATH=tmp_introot+'/bin:'+tmp_list+tmp_PATH
    else:
        tmp_PATH=tmp_list+tmp_PATH
else:
    if tmp_introot != '':
        tmp_PATH=tmp_introot+'/bin:'+tmp_PATH
 
if tmp_modpath == '1':
    tmp_PATH='../bin:'+tmp_PATH

if tmp_windbase != 'NOT_DEFINED':
    tmp_PATH=tmp_PATH+':'+tmp_windbase+'/host/x86-linux/bin'

result_command=commands.getoutput('echo $PATH')
tmp_PATH=commands.getoutput('echo '+tmp_PATH+'')
command='PATH'
commands.getoutput('echo "'+command.ljust(23)+' = '+result_command+'<br>" >>  ./report.html')

if tmp_PATH != result_command:
    commands.getoutput('echo "<font color=#ff0000>'+my_string.ljust(23)+' = '+tmp_PATH+'</font><br>" >>  ./report.html')

##############################3
# LD_LIBRARY_PATH
#

commands.getoutput('echo " <br>" >> ./report.html')
if tmp_os == 'Linux':
   tmp_LD_LIBRARY_PATH='$DDS_ROOT/lib:$ACE_ROOT/lib:$PYTHON_ROOT/lib:$OMNI_ROOT/lib:$ALMASW_INSTDIR/boost/lib:$TCLTK_ROOT/lib:'
   tmp_gnuroot=commands.getoutput('echo $GNU_ROOT')
   if (tmp_gnuroot != '/usr'):
     tmp_LD_LIBRARY_PATH=tmp_LD_LIBRARY_PATH+'$GNU_ROOT/lib:'
   tmp_LD_LIBRARY_PATH=tmp_LD_LIBRARY_PATH+'$ORIG_LD_LIBRARY_PATH'

#!#   if ((tmp_vltroot != '') and (tmp_vltroot != tmp_intlistPath)):
#!#      tmp_LD_LIBRARY_PATH='$VLTROOT/lib:'+tmp_LD_LIBRARY_PATH

#!#   if ((tmp_acsroot != '') and (tmp_vltroot != tmp_acsroot)):
   if ((tmp_acsroot != '') and (tmp_acsroot != tmp_intlistPath)):
      tmp_LD_LIBRARY_PATH='$ACSROOT/lib:'+tmp_LD_LIBRARY_PATH 
else:
    LD_LIBRARY_PATH='IMPOSSIBLE_TO_DETERMINE_THE_SYSTEM_NAME_OR_OS_NOT_SUPPORTED'

if tmp_intlist[0] != '':
    tmp_list=''
    for item_list in tmp_intlist:
        tmp_list=tmp_list+item_list+'/lib:'
  
    if tmp_introot != '':
        tmp_LD_LIBRARY_PATH=tmp_introot+'/lib:'+tmp_list+tmp_LD_LIBRARY_PATH
    else:
        tmp_LD_LIBRARY_PATH=tmp_list+tmp_LD_LIBRARY_PATH
else:
    if tmp_introot != '':
        tmp_LD_LIBRARY_PATH=tmp_introot+'/lib:'+tmp_LD_LIBRARY_PATH
   
if tmp_modpath == '1':
    tmp_LD_LIBRARY_PATH='../lib:'+ tmp_LD_LIBRARY_PATH

if tmp_windbase != 'NOT_DEFINED':
    tmp_LD_LIBRARY_PATH=tmp_LD_LIBRARY_PATH+':'+tmp_windbase+'/host/x86-linux/lib'


result_command=commands.getoutput('echo $LD_LIBRARY_PATH')
tmp_LD_LIBRARY_PATH=commands.getoutput('echo '+tmp_LD_LIBRARY_PATH+'')
command='LD_LIBRARY_PATH'
commands.getoutput('echo "'+command.ljust(23)+' = '+result_command+'<br>" >>  ./report.html')

if tmp_LD_LIBRARY_PATH != result_command:
    commands.getoutput('echo "<font color=#ff0000>'+my_string.ljust(23)+' = '+tmp_LD_LIBRARY_PATH+'</font><br>" >>  ./report.html')
   
commands.getoutput('echo " " >> ./report.html')
commands.getoutput('echo "<h1>*************************************<br>" >> ./report.html')
commands.getoutput('echo "********* /etc/hosts content ********<br>" >> ./report.html')
commands.getoutput('echo "*************************************</h1>" >> ./report.html')
commands.getoutput('echo " <br>" >> ./report.html')
if os.access('/etc/hosts',os.F_OK) == True:
    tmpFile = open('/etc/hosts','r')
    s = tmpFile.readlines()
    tmpFile.close()
    for line in s:
        commands.getoutput('echo "'+line+'<br>" >>  ./report.html')
else:
    commands.getoutput('echo "<font color=#ff0000> File does not exist! </font><br>" >>  ./report.html')

##############################################
# RPM Analysis
# If not switched off and if standard redhat
#
if ((rpmcheck == True) and (standardRedHat ==  True)):
    commands.getoutput('echo " <br>" >> ./report.html')
    commands.getoutput('echo "<h1>************************<br>" >> ./report.html')
    commands.getoutput('echo "***** RPM Analysis *****<br>" >> ./report.html')
    commands.getoutput('echo "************************</h1>" >> ./report.html')
    commands.getoutput('echo " <br>" >> ./report.html')

    package_file=open(rpmrefPath,'r')
    mylines=package_file.readlines()

    for myline in mylines:
        myline = myline.strip()
        not_installed=0
        not_same_version=0
        try:
            pack_name, pack_ver, pack_rel = string.split(myline)
        except ValueError, ex:
            logging.warning('Cannot split %s: %s' %(myline, ex))
            continue
        expected_package=pack_name+'-'+pack_rel+'-'+pack_ver
        # Special case : could have two versions of the same package installed
        status = commands.getstatusoutput('rpm -q ' + pack_name )[0]
        package_nr = commands.getoutput('rpm -q ' + pack_name + ' | wc -l')
        #print "STATUS = " + str(status)
        if status: 
            # Package is not installed
            commands.getoutput(
                   'echo "<font color=#ff0000>===> Package ' +
                   pack_name +
                   ' IS NOT INSTALLED</font><br>"  >>  ./report.html')
            commands.getoutput('echo "<br>"   >>  ./report.html')
        elif int(package_nr) > 1:
            # More than 1 rpm with the same name
            commands.getoutput(
                   'echo "<font color=#ff0000>===> There are  ' + 
                   package_nr + 
                   ' version of the package ' + 
                   pack_name +
                   'installed</font><br>"  >>  ./report.html')
            for i in commands.getoutput('rpm -q ' + pack_name).split('\n'):
                commands.getoutput(
                       'echo "<font color=#ff0000>=========> ' +
                       i +
                       '</font><br>"  >>  ./report.html')
            commands.getoutput(
                   'echo "<font color=#ff0000>===> Expected Version is : ' +
                   expected_package + 
                   '<font><br>"  >>  ./report.html')
        else:
            # standard situation, 1 package installed
            installed_package=commands.getoutput('rpm -q ' + pack_name)
            command="rpm -q --queryformat '%{NAME} %{RELEASE} %{VERSION}' " + pack_name
            commands.getoutput(command)
            installed_pack_name, installed_pack_rel, installed_pack_ver = \
                    commands.getoutput(command).split()
            installed_package=commands.getoutput('rpm -q ' + pack_name)
            if pack_rel != installed_pack_rel or pack_ver != installed_pack_ver:
                # Package is installed but with the wrong version
                commands.getoutput(
                     'echo "' +
                     'Package '.ljust(23) +
                     ' = ' +
                     installed_package +
                     '<br>" >>  ./report.html')
                commands.getoutput(
                     'echo "<font color=#ff0000>' +
                      '===> Expected Version'.ljust(23) +
                      '   ' +
                      expected_package +
                      '</font><br>" >>  ./report.html')
                commands.getoutput('echo " <br>" >> ./report.html')
            else:
                # Package is installed with the right version and release. 
                commands.getoutput(
                      'echo "<font color=#000000>' +
                      'Package '.ljust(23) +
                      ' = ' + 
                      installed_package +
                      '</font><br>" >>  ./report.html')
            #sys.exit(-1)
    package_file.close()

#############################
# Finishing the html file
# 

commands.getoutput('echo "</font>" >> ./report.html')
commands.getoutput('echo "</boby>" >> ./report.html')
commands.getoutput('echo "</html>" >> ./report.html')

print ' '
print 'The report file <report.html> has been created.'
print ' '

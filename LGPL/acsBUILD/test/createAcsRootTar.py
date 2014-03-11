#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# Copyright (c) European Southern Observatory, 2013 
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
#
# who       when      what
# --------  --------  ----------------------------------------------
# acaproni  2013-08-21  created
#
import os
import sys
from datetime import datetime
from subprocess import call

# The packages to include in the tar
acsRootPackages = (
                      "ACSSW",
                      "acsdata")

def getAcsVersionFromACSROOT():
    '''
    Get the ACS version out of ACSROOT.
    
    Raise a exception in case of error
    '''
    if os.environ.has_key('ACSROOT'):
        acsRoot=os.environ['ACSROOT']
    else:
        raise Exception("ACSROOT not defined!")
    # Assumes ACSROOT to be something like /alma/ACS-12.0/ACSSW
    strs=acsRoot.split("/")
    if len(strs)!=4:
        raise Exception("Malformed ACSROOT: "+acsRoot)
    temp=strs[2].replace(".","_")
    return temp

def getTagFromFile(tagFileName):
    '''
    getAcsTag() and getArchiveTag() both need to read the tag
    in the same way but from different files in $ACSROOT
    
    This method return the tag from the file with the passed name
    (i.e. ACS_TAG or ARCHIVE_TAG)
    
    @param tagFileName: the name of the file with the TAG to open
    @return the TAG contained in the file
    @throws exception in case of failure
    '''
    if  tagFileName is None:
        raise Exception("Invalid empty file name")
    
    if os.environ.has_key('ACSROOT'):
        acsRoot=os.environ['ACSROOT']
    else:
        raise Exception("ACSROOT not defined!")
    
    if acsRoot[len(acsRoot)-1] is not '/':
        fileName= acsRoot+"/"
    else:
        fileName= acsRoot
    
    fileToOpen=fileName+tagFileName
    # CHeck if the file exists and is readable
    if not os.access(fileToOpen, os.R_OK):
        raise Exception(fileToOpen+" unreadable")
    
    with open(fileToOpen) as f:
        content = f.readlines()
    
    if len(content) is not 1:
       raise Exception(fileToOpen+" is malformed") 
    else:
        return str(content[0]).strip()

def getACSVersionFromFile():
    '''
    @return the ACS version from $ACSROOT/ACS_VERSION
    '''
    return getTagFromFile("ACS_VERSION")
   
def getAcsTag():
    '''
    Get ACS tag from $ACSROOT/ACS_TAG
    
    @return: The tag or  exception in case of error
    '''
    return getTagFromFile("ACS_TAG")
    
def getArchiveTag():
    '''
    Get ACS tag from $ACSROOT/ARCHIVE_TAG
    
    @return: The tag or  exception in case of error
    '''
    return getTagFromFile("ARCHIVE_TAG")

def checkInstalledVersion(acsRoot,acsVersion):
    '''
    Check if the installed version of ACS in $ACSROOT matches with 
    the content of ACS_VERSION
    
    @return True if the version matches
    '''
    temp=acsVersion.replace(".","_")
    return acsRoot=="ACS-"+temp

def getAcsDistributionFolder():
    """
    Get the folder where all the the packages reside
    
    It is relative to the root i.e. something like alma/ACS-12.0/
    """
    if os.environ.has_key('ACSROOT'):
        acsRoot=os.environ['ACSROOT']
    else:
        raise Exception("ACSROOT not defined!")
    temp=acsRoot.replace("ACSSW", "")
    temp=temp.replace("/", "",1)
    return temp

def getDate():
    '''
    Return the date to be set in the name of the tar
    '''
    now=datetime.today()
    return "{0}{1:02}{2:02}".format(now.year,now.month,now.day)

def getArchitecture():
    '''
    Return the architecture 32/64 bit
    '''
    un=os.uname()
    return un[4]

def getDistribution():
    '''
    Get the distribution (RHEL, SL and so on)
    by parsing /etc/redhat-release
    
    @return: a string describing the distribution like RH6.5
             or UNKNOWN if /etc/redhat-release does not exist or is unreadable.
             
    '''
    ret="UNKNOWN"
    fname="/etc/redhat-release"
    with open(fname) as f:
        content = f.readlines()
    distr=str(content)
    if distr.find("Red Hat Enterprise Linux")>=0:
        temp="RH"
    elif distr.find("Scientific Linux")>=0:
        temp="SL"
    else:
        return ret
    parts=distr.split(" ")
    version=parts[len(parts)-2]
    return temp+version

def buildTarName(acs,archive):
    '''
    Build the name of the tar
    
    @param acs: ACS tag
    @param archive: ARC tag or None if ARCHIVE not installed
    '''
    date=getDate()
    architecture=getArchitecture()
    dist=getDistribution()
    
    ret=acs
    if archive is not None:
        ret=ret+"-"+archive.replace("-B","")

    return ret+"-"+date+"-"+architecture+"-"+dist+".tar.gz"

if __name__=="__main__":
    
    try:
        acsTag=getAcsTag()
    except Exception as e:
         # No ACS TAG means something strange: abort
         print "Error reading ACS TAG",e
         exit(-1)
    
    try:
        arcTag=getArchiveTag()
    except Exception as e:
        # This is not an error but means that ARCHIVE is not installed in ACSROOT
        arcTag=None
    print "ACS_TAG is",acsTag
    if arcTag is not None:
        print "ARCHIVE_TAG is",arcTag
    else:
        print "No ARCHIVE TAG found (ARCHIVE not installed in ACSROOT)"
    
    acsVersionFromFile=getACSVersionFromFile()
    acsVersionFromACSROOT=getAcsVersionFromACSROOT()
    
    if not checkInstalledVersion(acsVersionFromACSROOT,acsVersionFromFile):
        print "ACS version seems inconsistent:",acsVersionFromACSROOT,"does not match with",acsVersionFromFile
        exit(1)
    
    print "ACS version is",acsVersionFromACSROOT

    currentFolder=os.getcwd()
    srcFolder=getAcsDistributionFolder()
    
    print "Creating ACSROOT tar for", acsVersionFromACSROOT,"in",currentFolder
    tarName=buildTarName(acsVersionFromACSROOT,arcTag)
    print "Tar name",tarName
    tarNameFullPath=currentFolder+"/"+tarName
    
    # Go to root /
    os.chdir("/")
    print "Moved into",os.getcwd()
      
    
    # build the command to pass to call
    
    cmd=[]
    cmd.append("tar")
    cmd.append("cpzf")
    cmd.append(tarNameFullPath)
    cmd.append("--exclude")
    cmd.append("Sources")
    for pkg in acsRootPackages:
        cmd.append(srcFolder+pkg)
    print "Running tar....",
    call(cmd)
    
    # Back to original folder
    os.chdir(currentFolder)
    print "Done"

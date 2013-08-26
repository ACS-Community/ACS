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

def getAcsVersion():
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
    temp=strs[2].replace(".","")
    temp=temp.replace("-","")
    while len(temp)<7:
        temp=temp+"0"
    return temp

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

if __name__=="__main__":
    acs=getAcsVersion()
    date=getDate()
    arch=getArchitecture()
    currentFolder=os.getcwd()
    srcFolder=getAcsDistributionFolder()
    print "Creating ACSROOT tar for", acs,"in",currentFolder
    tarName=acs+"-ACSROOT-"+date+"-"+arch+".tar.gz"
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
    for pkg in acsRootPackages:
        cmd.append(srcFolder+pkg)
    print "Running tar....",
    call(cmd)
    
    # Back to original folder
    os.chdir(currentFolder)
    print "Done"

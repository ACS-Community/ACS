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
import sys
import os
import re
from optparse import OptionParser
from subprocess import call

"""Build a release:
    1. check out and build ACS
    2. if requested, checkout and build ARCHIVE
"""

def getArchitecture():
    '''
    Return the architecture 32/64 bit
    '''
    un=os.uname()
    return un[4]

def printHelp(name):
    print name,"-l|--lgpl -n|--nolgpl -t|--tag ACSTAG -v|--version ACSVERSION [-a|--archive ARCHIVETAG] [-h|--help]"
    print "-h|--help print this help and exit"
    print "-l|--lgpl: build ACS LGPL version"
    print "-n|--nolgpl: build ACS NO-LGPL version"
    print "-t|--tag ACSTAG: the ACS tag to check out to build the release"
    print "-v|--version: the ACS version to build like 12.1.0"
    print "-a|--archive ARCHIVETAG: the ARCHIVE tag to check out to build the subsystem"
    print "                         (if no present ARCHIVE is not built)"
    print "Tags are in SVN format like for example trunk/ACS or branches/ACS-12_1-B or tags/ACS-12_1_0-TEMP"
    print "The version is used to double check what the tool is building against ACS_VERSION and ACS_PATCH_LEVEL"
    print "checked out from the repository."
    print "Prerequisite: ExtProd must have already been built before running this tool\n"
    
def integrityChecks(version, lgpl):
    """
    Run several integrity checks like checking if ACS root is defined and
    so on.
    
    @param version ACS version like 12.1.0
    @param lgpl True if ACS LGPL version must be built
    @return True if everything seems ok and False otherwise
    """
    try:
        acsRoot=os.environ["ACSROOT"]
        print "\tACSROOT",acsRoot
    except:
        print "ACSROOT not defined!"
        return False;
    
    try:
        instDir=os.environ["ALMASW_INSTDIR"]
        print "\tALMA sw installation folder",instDir
    except:
        print "ALMA installation folder (ALMASW_INSTDIR) not defined!"
        return False
    
    arc=getArchitecture();
    print "\tCurrent architecture",arc
    
    #Check if RTI is installed
    #try:
    #    dds=os.environ["NDDSHOME"]
    #    print "\tRTI DDS found in",dds
    #except:
    #    print "RTI DDS not installed"
    #    return False
    
    # Check if real time is installed
    #if arc!="x86_64" and not lgpl:
        #Read ALMASW_INSTDIR and check if rtai and rtlinux exist
    #    if not os.path.exists(instDir+"/rtai"):
    #        print instDir+"/rtai not found!"
    #        return False
    #    if not os.path.exists(instDir+"/rtlinux"):
    #        print instDir+"/rtlinux not found!"
    #        return False
    #    print "\trtai and rtlinux found"
    
    #Check the passed version against ALMASW_INSTDIR
    # Passed version is in the form 12.1.0
    parts=version.split('.')
    if not instDir.endswith(parts[0]+'.'+parts[1]):
        print "Installation dir",instDir,"mismatch with ACS release to build",version
        return False
    
    # Is ACSROOT= ALMASW_INSTDIR+"/ACSROOT"?
    if not acsRoot==instDir+"/ACSSW":
        print "ACSROOT ("+acsRoot+") does not match with ALMASW_INSTDIR ("+instDir+")"
        return False
    
    # All tests passed
    return True
    
def checkAcsVersion(version):
        """Check the passed version (for example 12.1.0) against the files generated 
        by checking out ACS like ACS_VERSION and ACS_PATCH_LEVEL.
        
        @param version ACS version like 12.2.1
        @return True if the passed version matched with the checked out version
        """
        try:
            acsVersionFromFile = open('ACS_VERSION', 'r').read().strip()
        except:
            print "Error opening/reading ACS_VERSION"
            return False
        try:
            acsRevisionFromFile = open('ACS_PATCH_LEVEL', 'r').read().strip()
        except:
            print "Error opening/reading ACS_PATCH_LEVEL"
            return False
        print "ACS version checked out from SVN is",acsVersionFromFile,"revision",acsRevisionFromFile
        return version==acsVersionFromFile+"."+acsRevisionFromFile
    
def checkOutACS(svnUrl,acsTag,lgpl,version):
    """
    Check ACS from SVN
    @param svnUrl: SVN base URL
    @param acsTag: the trunk, tag or branch to check out
    @param lgpl: if True checkout LGPL otherwise NO-LGPL version of ACS
    @param version ACS version like 12.1.0
    
    @return: False in case of error; True otherwise
    To check out ACS it checks out the ACS folder then run either
    make svn-get-lgpl or svn-get-no-lgpl
    """
    svnPath=svnUrl+"/"+acsTag
    cmd=[]
    cmd.append("svn")
    cmd.append("co")
    cmd.append(svnPath)
    cmd.append("--depth=files")
    print "Checking out ACS from",svnPath
    ret=call(cmd)
    
    if (ret==0):
        print "Checkout of empty ACS folder succeeded"
    else:
        print "ERROR",ret,"checking out ACS - SVN returned",ret
        return False
        
    # Now run make
    print "=====>",os.getcwd()
    os.chroot("ACS")
    print "Changed folder to",os.getcwd()
    
    if not checkAcsVersion(version):
        return False
    
    outFile=open("checkOutACSWithMake.log","w")
    makeCmd=[]
    makeCmd.append("make")
    if (lgpl):
        makeCmd.append("svn-get-lgpl")
    else:
        makeCmd.append("svn-get-no-lgpl")
    ret=call(makeCmd,stdout=outFile,stderr=outFile)
    outFile.close()
    print "make returned",ret
    return ret==0

def buildAcs():
    """
    Build ACS running 'nohup make build_clean'
    redirects the output to a file.
    
    @return False in case of error
    """
    outFile=open("makeBuild.log","w")
    print "Building ACS. Output in "+os.getcwd()+"/makeBuild.log"
    makeCmd=[]
    makeCmd.append("nohup")
    makeCmd.append("make")
    makeCmd.append("build_clean")
    ret=call(makeCmd,stdout=outFile,stderr=outFile)
    outFile.close()
    print "make returned",ret
    if (ret!=0):
        return False
    # Check if the log contains failed
    inF=open("makeBuild.log","r")
    content=inF.readlines()
    inF.close()
    for line in content:
        str=line.strip().upper()
        if str.find("FAILED")!=-1:
            print "At least one ACS module failed to build. Check output in",os.getcwd()+"/makeBuild.log"
            return False
    return True

if __name__=="__main__":
    # Parse the command line
    parser = OptionParser()
    parser.add_option("-l", "--lgpl", action="store_true", dest="LGPL", help="build ACS LGPL version", default=False)
    parser.add_option("-n", "--nolgpl",action="store_true", dest="NOLGPL", help="build ACS NO-LGPL version", default=False)
    parser.add_option("-t", "--tag", dest="ACSTAG", help="the ACS tag to check out to build the release", default=None)
    parser.add_option("-v", "--version", dest="ACSVERSION", help="the ACS version to build like 12.1.0", default=None)
    parser.add_option("-a", "--archive", dest="ARCHIVETAG", help="the ARCHIVE tag to check out to build the release", default=None)
    (options, args) = parser.parse_args()

    if (options.LGPL==options.NOLGPL):
        print "\nERROR: Build either LGPL or NO-LGPL\n"
        printHelp(sys.argv[0])
        sys.exit(-1)
    if (options.ACSTAG==None):
        print "\nERROR: provide a TAG for ACS\n"
        printHelp(sys.argv[0])
        sys.exit(-1)
    if (options.ACSVERSION==None):
        print "\nERROR: provide the ACS version like 12.1.0\n"
        printHelp(sys.argv[0])
        sys.exit(-1)
    if not re.match("[0-9]+\.[0-9]+\.[0-9]+",options.ACSVERSION):
        print "\nThe passed version",options.ACSVERSION,"wrong format; 12.1.0 is a correct example!"
        printHelp(sys.argv[0])
        sys.exit(-1)
        
    # Print what the script is going to do
    print "Building ACS release",options.ACSVERSION
    print "Check out ACS from tag",options.ACSTAG
    if options.LGPL:
        print "LGPL",
    else:
        print "NO-LGPL",
    print "ACS flavor"
    if options.ARCHIVETAG==None:
        print "ARCHIVE will NOT be built"
    else:
        print "ARCHIVE tag to build is",options.ARCHIVETAG
    
    # SVN URL
    try:
        SVN_URL=os.environ["SVN_URL"]
    except:
        SVN_URL="https://alma-svn.hq.eso.org/p2"
        print "SVN_URL environment variable not found: using hardcoded value",SVN_URL
     
    print "Performing integrity checks"    
    if not integrityChecks(options.ACSVERSION, options.LGPL):
        sys.exit(-1)
    print "System ok."
    
    cwd=os.getcwd();
    
    # Checkout ACS
    if not checkOutACS(SVN_URL,options.ACSTAG,options.LGPL,options.ACSVERSION):
        print "Error reported checking out ACS"
        os.chdir(cwd)
        sys.exit(-1)
        
    if not buildAcs():
        sys.exit(-1)
    
    cwd=os.getcwd();
    

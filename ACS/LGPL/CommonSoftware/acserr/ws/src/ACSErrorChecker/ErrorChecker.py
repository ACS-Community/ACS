#!/usr/bin/env python
################################################################################################
# @(#) $Id: ErrorChecker.py,v 1.13 2011/10/28 14:39:18 hsommer Exp $
#
#    ALMA - Atacama Large Millimiter Array
#    (c) Associated Universities, Inc. Washington DC, USA, 2001
#    (c) European Southern Observatory, 2002
#    Copyright by ESO (in the framework of the ALMA collaboration)
#    and Cosylab 2002, All rights reserved
#
#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU Lesser General Public
#    License as published by the Free Software Foundation; either
#    version 2.1 of the License, or (at your option) any later version.
#
#    This library is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    Lesser General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public
#    License along with this library; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
###############################################################################

# The script executes the scan of the source directory of a subsystem
#
# TODO: Generate a tree of web pages with the data of all the subsystems

import sys, string, os
import xml.dom.minidom
from ACSErrorChecker.Subsystem import *

#Each subsystem is stored as a dictionary
# with the followings keys:
#   Name
#   ReportFormat
#   BaseDir
#   Min
#   Max
# These are the parameters needed to build a Subsystem object
    
######################## writeErrorOnHTML ################################

def writeErrorOnHTML(fileName,subsystemName,errorMsg,theException):
    text = '<HTML><HEAD></HEAD><BODY>\n'
    text=text+'<h1 align="center">Error definitions for '+subsystemName+'</h1>\n'
    text=text+'<HR size="3" width="80%" align="center">\n'
    text=text+'<P>The analyzer failed scannig this system reporting the following message:</P>\n'
    text=text+'<P align="center"><I>'+errorMsg+'</I></P>\n'
    text=text+'<P>Exception caught:</P>'
    text=text+'<VERBATIM>'+theException+'</VERBATIM>'
    text=text+'<P>Please report this error to the <I>Software Engineering team</I></P>\n'
    text=text+'</BODY></HTML>\n'
    if fileName==None:
        print text # To stdout
    else:
        try:
            outF=open(fileName,"w+")
            outF.write(text)
            outF.flush()
            outF.close
        except IOError, e:
            print "Unable to open ",fileName, "for output"
            print "Exception",e
            return
        
######################## parseConfigFile###############################
def parseConfigFile(fileName,repDir,incDirs,excDirs,testRng,systems):
    """Parse the command file filling the variables
    return 0 if the parsing was ok

    fileName: the config file to scan
    repDir: the directory to write the reports on
    incDirs: the include directories
    excDirs: the folders to exclude form search
    testRng: the range of test and example errors
    systems: the subsystems"""
    #parse the config file
    if not os.access(fileName,os.R_OK):
        print "Configuration file not found or unreadable!"
        return -1;
    doc=xml.dom.minidom.parse(fileName)
    
    if doc.documentElement.tagName!="ErrorChecker":
        print "Error parsing the configuration file",fileName
        print "Document type found",doc.documentElement.tagName,
        print "instead of ErrorChecker"
        return -1
    
    mainElement = doc.getElementsByTagName("ErrorChecker")
    if (len(mainElement)>1):
        print "Error parsing the configuration file",fileName
        print "Found",len(mainElement),"elements of type ErrorChecker"
        print "instead of 1"
        return -1
    
    if (len(doc.getElementsByTagName("ReportDir"))>1):
        print "Error parsing the configuration file",fileName
        print "Found",len(doc.getElementsByTagName("ReportDir")),"elements of type ReportDir"
        print "instead of 1"
        return -1
    if (len(doc.getElementsByTagName("ReportDir"))==1):
        repDirNode = doc.getElementsByTagName("ReportDir")[0]
        repDir.append(repDirNode.attributes.get("path").value)
    else:
        #Default value is ".", the current directory
        repDir.append(".")
    
    searchDirs = doc.getElementsByTagName("SearchDir")
    for searchDir in searchDirs:
        incDirs.append(searchDir.attributes.get("DirName").value)
    
    forbiddenDirs = doc.getElementsByTagName("ExcludeDir")
    for forbiddenDir in forbiddenDirs:
        excDirs.append(forbiddenDir.attributes.get("DirName").value)
    
    testRanges = doc.getElementsByTagName("ExampleAndTestRange")
    testRange =doc.getElementsByTagName("ExampleAndTestRange")[0]
    minTestRanges = int(testRange.attributes.get("Min").value)
    maxTestRanges = int(testRange.attributes.get("Max").value)
    testRng["Min"]=int(minTestRanges)
    testRng["Max"]=int(maxTestRanges)
    
    subsystems = doc.getElementsByTagName("SubSystem")
    for subsys in subsystems:
        subsystem = {}
        subsysRepFormat = subsys.attributes.get("ReportFormat").value
        subsysName = subsys.attributes.get("Name").value
        subsysBaseDir = subsys.attributes.get("BaseDir").value
        subsysMin = subsys.attributes.get("Min").value
        subsysMax = subsys.attributes.get("Max").value
        subsystem["ReportFormat"]=subsysRepFormat
        subsystem["Name"]=subsysName
        subsystem["Min"]=int(subsysMin)
        subsystem["Max"]=int(subsysMax)
        subsystem["BaseDir"]=subsysBaseDir
        systems.append(subsystem)

######################## check ####################################

def checkWithConfigFile(configFile):
    """
    Read the configuration file from the parameter and create the reports
    into the given directory with the given format
    
    Return values
        0: ok
        -1: error in the config file
        n: number of errors ecountered while scanning the subsystems
    """
    # The list of the subsystems to scan
    SubSystems=[]
    
    # The directory where the reports will be written
    # The default value is "." and is set in the parse function
    # if the entry is not found in the configuration file
    ReportDir=[]
    
    # The list of directory to exclude while looking for 
    # the error files
    # (if a direcotory iin the list is found the process to not 
    # enter the tree to look for error files)
    # It speedS up the search
    ExcludeDirs = []
    
    # The directories that could contain xml files
    # No influence in the recursive searcH for the error files
    # It speedS up the search
    IncludeDirs = []
    
    # The range of numbers allocated for tests and examples
    #
    # NOTE: these errors can be duplicated (i.e. no check has to be done
    #       for that kind of errors)
    ExaplesAndTestRange = { 
        "Min": None,
        "Max": None
        }
    
    parseRet = parseConfigFile(configFile,ReportDir,IncludeDirs,ExcludeDirs,ExaplesAndTestRange,SubSystems)
    if parseRet==-1:
        return -1
    
    # The value to report while exiting (0 means that all the subsystem
    # were checked without errors
    nErrors=0
    
    # Generate the report for each subsystem
    for subs in SubSystems:
      try:
        subError=0
        subystemObj = Subsystem(
                     subs["Name"],
                     subs["BaseDir"],
                     subs["Min"],
                     subs["Max"],
                     ExaplesAndTestRange,
                     ExcludeDirs,
                     IncludeDirs)
        subError=1
        if subs["ReportFormat"]=="html":
          subystemObj.generateHTMLPage(ReportDir[0]+"/"+subs["Name"]+".html")
        else:
          subystemObj.printErrors();
    
      except Exception, inst:
        print inst
        nErrors=nErrors+1
        if subError==0:
          writeErrorOnHTML(ReportDir[0]+"/"+subs["Name"]+".html",subs["Name"],"Subsystem processing failure",str(inst))
        else:
          writeErrorOnHTML(ReportDir[0]+"/"+subs["Name"]+".html",subs["Name"],"The generation of the report failed",str(inst))
    
    return nErrors

def check():
    """Read the configuration file to scan the subsystem and create the reports
    into the given directory with the given format
    It checks for the configuration file and call the checkWithConfigFile function
    
    Return values
        0: ok
        -1: error in the config file
        n: number of errors ecountered while scanning the subsystems"""
    # Look for the config file.
    # It may be
    #   1. a parameter in the command line
    #   2. ErrorChecker.xml in the current directory
    #   3. ErrorChecker.xml in the home directory
    ConfigFileName=None
    #Check if a parameter in the command line exists
    if len(sys.argv)>1:
        ConfigFileName=sys.argv[1]
    else:
        if os.access("./ErrorChecker.xml",os.R_OK):
            ConfigFileName="./ErrorChecker.xml"
        else:
            if os.access("~/ErrorChecker.xml",os.R_OK):
                ConfigFileName="~/ErrorChecker.xml"
    
    if ConfigFileName==None:
        print "Error: config file not found!"
        return -1
    return checkWithConfigFile(ConfigFileName)

def printUsage():
    """Print the usage string in the stdout"""
    print "Usage:",sys.argv[0],"[configFile]"
    print "config file: the path name of the configuration file"
    print "If configFile is not present, the tool looks for:"
    print "\t./ErrorChecker.xml"
    print "\t~/ErrorChecker.xml\n"

################################ MAIN ##########################

if __name__ == "__main__":
    if len(sys.argv)==1:
        # No args in the cmd line
        check()
    elif len(sys.argv)==2:
        # The config file is in the cmd line
        if os.access(sys.argv[1],os.R_OK):
            # The file is readable!
            checkWithConfigFile(sys.argv[1])
        else:
            # Uh-oh The file is unreadable :-o
            print sys.argv[1],"not found!"
            sys.exit(-1)
    else:
        # Wrong number of args in command line
        printUsage()
        sys.exit(-1)
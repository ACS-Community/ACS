#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# Copyright (c) European Southern Observatory, 2016 
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
# acaproni  2016-05-24  created
#
from optparse import OptionParser
from subprocess import call
import os
import sys
import datetime

"""
This python script generates documentation for ACS java sources
by invoking javadoc

The script takes the following parameters:
   * src folder:  the folder containing java sources (usually ACS as checked out from the repository)
   * dest folder: the folder where javadoc put the HTMLs
   
"""

def buildClasspath():
    '''
    Buils the classpath for referenced classes
    '''
    classpath=''
    # Add all jars from ACSROOT/lib
    acsRoot=os.environ['ACSROOT']
    acsRootLib=acsRoot+os.path.sep+"lib"
    files = next(os.walk(acsRootLib))[2]
    for file in files:
        if file.endswith(".jar"):
            classpath=classpath+file+os.path.pathsep
    
    # Add jars from JACORB_HOME/lib
    jacorbHome=os.environ['JACORB_HOME']
    jacorbHomeLib=jacorbHome+os.path.sep+"lib"
    files = next(os.walk(jacorbHomeLib))[2]
    for file in files:
        if file.endswith(".jar"):
            classpath=classpath+file+os.path.pathsep
    # Add jars from JACORB_HOME/lib/endorsed
    jacorbHomeLibEndorsed=jacorbHomeLib+os.path.sep+"endorsed"
    files = next(os.walk(jacorbHomeLibEndorsed))[2]
    for file in files:
        if file.endswith(".jar"):
            classpath=classpath+file+os.path.pathsep
    return classpath

def containsJavaSources(folder):
    '''
    @param folder: the folder (src or test) to check if contains java sources
    @return: True if the passed folder contains java sources
    ''' 
    for root, subdirs, files in os.walk(folder):
        for file in files:
            if file.endswith(".java"):
                return True
    return False

def getSrcPaths(sourceFloder, includeTestFolder):
    """
    Scan the source folder and return a list of source folders
    containg java files.
    Java source can be contained into src or test (the latter is used only 
    if the includeTestFolder parameter is True)
    The search is recursive because a folder can contains several modules
    
    @param sourceFloder: root source folder (generally ACS, passed int he command line)
    @param includeTestFolder: True to inculde test folders in the scan 
    """
    ret = []
    print os.path.sep
    for root, subdirs, files in os.walk(sourceFloder):
        if root.endswith(os.path.sep+"src") or (includeTestFolder and root.endswith(os.path.sep+"test")):
            if containsJavaSources(root):
                ret.append(root)
    return ret

def getJavaPackagesRoot(srcFolders):
    """
    Get the root folders of the java packages in the passed list of folders
    @param srcFolders: the list of folders containg java sources
    @return:  the root of java packages (something like, com, alma..)
    """
    ret = []
    for folder in folders:
        directories = next(os.walk(folder))[1]
        for directory in directories:
            if containsJavaSources(folder+os.sep+directory):
                if ret.count(directory)==0:
                    print "Adding",directory
                    ret.append(directory)
    return ret

def buildJavadocCmd(dest,srcFolders,pkgs):
    '''
    Build the command to invoke javadoc with subprocess.call.
    javadoc must be invoked with a command line like
    javadoc -d ./html -splitIndex -windowtitle 'ACS 2016.4' -J-Xmx180m -sourcepath ./src -subpackages alma:com
    
    @param dest The destination folder where javadoc creates html
    @param srcFolders The folders containing java sources
    @param pkgs The package roots (alma, com, cern..)
    @return: a list of parameteres to invoke javadoc 
    '''
    sources = ''
    for folder in folders:
        sources = sources + folder+os.path.pathsep
    
    packages = ''
    for pkg in pkgs:
        packages = packages + pkg+os.path.pathsep  
    
    ret=[]
    ret.append("javadoc")
    ret.append("-d")
    ret.append(dest)
    ret.append('-splitIndex')
    ret.append('-windowtitle')
    ret.append(os.environ['ALMASW_RELEASE'])
    ret.append('-J-Xmx250m')
    ret.append('-sourcepath')
    ret.append(sources)
    ret.append('-subpackages')
    ret.append(packages)
    ret.append("-classpath")
    ret.append(buildClasspath())
    ret.append("-quiet") # Suppress non-warning non-error messages
    ret.append("-author") #Adds the author to generated docs
    ret.append("-doctitle")
    ret.append(os.environ['ALMASW_RELEASE']+" API documentation")
    ret.append("-header")
    ret.append('<EM>'+os.environ['ALMASW_RELEASE']+'</EM>')
    ret.append("-footer")
    ret.append('<EM>Generated at '+str(datetime.date.today())+'</EM>')
    ret.append("-charset")
    ret.append("iso-8859-15")
    return ret
        

if __name__=="__main__":
    # javadoc -d ./html -splitIndex -windowtitle 'ACS 2016.4' -J-Xmx180m -sourcepath ./src -subpackages alma:com
    
    # Parse the command line
    parser = OptionParser()
    parser.add_option("-d", "--destFolder", help="HTML destination folder", default=".",action="store", type="string", dest="destFolder")
    parser.add_option("-s", "--sourceFolder", help="ACS source folder", default=".",action="store", type="string", dest="srcFolder")
    parser.add_option("-t", "--test", help="Include java sources from test folders", action="store_true", dest="includeTest", default=False)
    (options, args) = parser.parse_args()
    
    print 
    
    # Check if src folder exists
    if not os.path.exists(options.srcFolder):
        print "The source folder", options.srcFolder,"does not exist"
        sys.exit(-1)
    elif not os.path.isdir(options.srcFolder):
        print "The source folder", options.srcFolder,"is unreadable"
        sys.exit(-1)
    else:
        print "Reading java sources from",options.srcFolder
        
    # Check if the destination folder exists
    if not os.path.exists(options.destFolder):
        print "The destination folder", options.destFolder,"does not exist"
        sys.exit(-1)
    elif not os.path.isdir(options.destFolder):
        print "The destination folder", options.destFolder,"is unreadable"
        sys.exit(-1)
    else:
        print "Writing javadoc into",options.destFolder
        
    # Look for src (and if it is the case test) folders containing java sources
    folders = getSrcPaths(options.srcFolder, options.includeTest)
    print "Found", len(folders),"folders containing java sources"
    print folders
    
    roots = getJavaPackagesRoot(folders)
    print roots
    
    # Run javadoc
    cmd=buildJavadocCmd(options.destFolder,folders,roots)
    print '-------------------------'
    for s in cmd:
        print s
    print '-------------------------'
    print "Running javadoc....",cmd
    call(cmd)
#
# ___oOo___

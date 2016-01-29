#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# Copyright (c) European Southern Observatory, 2015 
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
# acaproni  2015-02-17  created
#
import os

class MakefileHelper(object):
    '''
    MakefileHelper
    '''
    
    def __init__(self, filePath):
        '''
        Build a MakefileHelper
        '''
        if filePath is None or filePath is "":
            raise Exception("Invalid file path")
        if not os.access(filePath,os.R_OK):
            raise Exception(filePath+" is unreadable")
        
        self.path=filePath.strip()
        self.folder=self.path.split(os.path.sep+'Makefile')[0]
        
        # The number of jars built by the Makefile
        self.jarNames=self.getMakefileTagValue("JARFILES")
        
        # Associate to each jar the folder of java sources as set in the Makefile
        if len(self.jarNames)>0:
            self.javaPackages=self.readJavaPackages(self.jarNames)
        else:
            self.javaPackages=[]

    def get_path(self):
        return self.path


    def get_folder(self):
        return self.folder


    def get_java_packages(self):
        return self.javaPackages

    
    def getMakefileTagValue(self, tagName):
        '''
        Scan the Makefile to get the value associated to the passed tag.
        For example if the Makefile contains a line like
            TAGEXAMPLE = a b c 
        self.getMakefileTagValue('TAGEXAMPLE') returns ['a', 'b', 'c'] 
        
        @param tagName: the name of the tag
        @return: A list of values o of the passed TAG in the MAkefile
        '''
        if tagName is None or tagName=="":
            raise Exception(filePath+" is unreadable")
        ret=[]
        
        with open(self.path) as f:
            content = f.readlines()
        
        for line in content:
            str = line.strip()
            # Remove comments
            pos = str.find("#")
            if pos>-1:
                str=str[:pos]
            if str.startswith(tagName):
                if not str.count("=")==1:
                    # Malformed?
                    continue
                strs = str.split("=")
                jars = strs[1].strip()
                if jars.count(" ")>0:
                    tempStrs=jars.split(" ")
                    for tempStr in tempStrs:
                        ret.append(tempStr.strip())
                else:
                    ret.append(jars.strip())
                    
        return ret
        
        
    def readJavaPackages(self, jars):
        '''
        Read the java package of each jar file of the makefile
        from the <jarName>_DIRS Makefile tag
        
        @param The name of the jars read from the makefile: 
        @return A list of tuples i.e.  [(jarName, [package-paths])]
        '''
        if len(jars)==0:
            return [];
        ret=[]
        for jar in jars:
            pkgs=self.getMakefileTagValue(jar+"_DIRS")
            ret.append((jar,pkgs))
        return ret
    
    def getJarsNumber(self):
        '''
        @return: The number of jars built by this Makefile
        '''
        return len(self.jarNames)
    
#
# ___oOo___

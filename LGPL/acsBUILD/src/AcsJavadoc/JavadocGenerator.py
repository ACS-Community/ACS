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
import subprocess

class JavadocGenerator(object):
    '''
    JavadocGenerator creates the javadoc ACS java sources
    '''
    
    def __init__(self, makefileHelper):
        ''' 
        Constructor
        
        @paran makefileHelpr: the helper of the Makefile
        '''
        if makefileHelper is None:
            raise Exception("Invalid MakefileHelper instance")
        self.helper=makefileHelper
        self.packages=self.looksForJavaPackages(self.helper.get_java_packages())

    def looksForJavaPackages(self, pkgs):
        '''
        Recursively scans the folders and look for java sources.
        A folder containing a java sources is a java package to be passed
        to the javadoc executable
        
        @param pkgs: the list of tuples (jar name, java packages)
        @return: the list of java packages (i.e. folders containing java sources)
        '''
        ret = []
        actualWD=os.getcwd()
        for jar in pkgs:
            folders=jar[1]
            for folder in folders:
                os.chdir(self.helper.get_folder()+os.path.sep+folder)
                for root, dir, files in os.walk("."):
                    for file in files:
                        if file.lower().strip().endswith(".java"):
                            ret.append((folder+os.path.sep+root[2:].strip()).replace(os.path.sep,"."))
                            print root
                            break
        os.chdir(actualWD)         
        print ret
        return ret
    
    def getJavaPackages(self):
        '''
        Return the list of java packages to build the documentation
        '''
        return self.packages
#
# ___oOo___

#!/usr/bin/env python
################################################################################################
# @(#) $Id$
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
################################################################################################
'''
This script generate the documentation for the errors using ACSErrorChecker
It generates a configuration file then calls the ErrorChecker
'''
################################################################################################
import os
from sys import argv
from ACSErrorChecker import *;

if len(argv)!=3:
    print "Wrong number of args"
    os.exit(-1)
    
ScanDir=argv[1]
ReportDir = argv[2]

XMLConfigFile = """<?xml version="1.0" encoding="UTF-8"?>
<ErrorChecker>
    <ReportDir path="html"/>
    <SearchDir DirName="idl"/>
    <ExcludeDir DirName="CVS"/>
    <ExampleAndTestRange Min="900000" Max="909999" />
    <SubSystem Name="ACS" ReportFormat="html" Min="0" Max="9999" BaseDir="dir/"/>
</ErrorChecker>
"""

XMLConfigFile = XMLConfigFile.replace('<ReportDir path="html"/>','<ReportDir path="'+ReportDir+'"/>')
XMLConfigFile = XMLConfigFile.replace('BaseDir="dir/"','BaseDir="'+ScanDir+'/"')

print XMLConfigFile

#The name of the configuration file 
configFileName = "./ErrorChecker.xml"
# Write the config file
outF = file(configFileName,"w+")
outF.write(XMLConfigFile)
outF.flush()
outF.close()

# Scan the folder for errors
ret=ErrorChecker.checkWithConfigFile(configFileName)

# Remove the configuration file if there were no errors
# I leave the file here in case of errors for debugging
if ret==0:
    os.remove(configFileName)
else:
    sys.exit(ret)

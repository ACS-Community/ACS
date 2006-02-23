#!/usr/bin/env python

# @(#) $Id: makeRosewp.py,v 1.3 2004/06/16 15:42:44 gchiozzi Exp $
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

'''
makeRosewp.py:
    Publishes the rose model.
    Takes from the command line path to the complete rosewp command
    and the path of the current directory.
'''

import sys
import os
import re
import shutil
import commands

print "Run rose batch web publisher..."

# Name of the rosewp init file to produce
roseWpIni = "rosewp.ini"

# Default Rose Path
rosePath = "/cygdrive/c/Program Files/Rational/Rose"

# Batch rose publisher executable
rosewpExe = "/rosewp/rosewpbatch.exe"

# Current directory from environment (in cygwin format)
destPath = commands.getoutput("pwd")

# Rose path.
# Uses ROSE_HOME directory if defined or issues warning and uses default
if (os.environ.has_key("ROSE_HOME")):
   rosePath = os.environ["ROSE_HOME"]
else:
   print "WARNING: ROSE_HOME environment variable undefined. Using default: " + rosePath

# Replaces cygwin path characters with Windows
destPath = destPath.replace("/cygdrive/","")
destPath = destPath.replace("/",":\\",1)
destPath = destPath.replace("/","\\")

fileHandle = open(roseWpIni,"w")
print >>fileHandle, "[RoseWebPublisher]"
print >>fileHandle, "LevelOfDetail=2"
print >>fileHandle, "DiagramType=2"
print >>fileHandle, "PrintInherited=1"
print >>fileHandle, "PrintProperties=1"
print >>fileHandle, "IncludeAssociations=1"
print >>fileHandle, "Notation=2"
print >>fileHandle, "RootFileName=" + destPath + "\\html\\root.html"
print >>fileHandle, "Model=" + destPath + "\\ACS.mdl"
fileHandle.close()

#rosewp_cmd = '\"' + rosePath + rosewpExe + " " + destPath + roseWpIni + '\" ' 

rosewp_cmd = commands.mkarg(rosePath + rosewpExe) + " " + commands.mkarg(destPath + "\\" + roseWpIni) 

print rosewp_cmd
print commands.getoutput(rosewp_cmd)
print ".... done!"
#
# ___oOo___


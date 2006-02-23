#!/usr/bin/env python

# @(#) $Id: extractDiagrams.py,v 1.1 2003/12/31 08:18:17 gchiozzi Exp $
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
extractDiagrams.py:
    Searches a diagram HTML file produced by Rose Web Publisher
    and creates a copy of the actual diagram picture
    in the destination directory the the clean diagram name as file name.
    Following substitution are done in the file name:
        re.sub(r':&nbsp;','-')
        re.sub(r'&nbsp;/&nbsp;','-')
        re.sub(r'/','-')
        re.sub(r'&nbsp;','_')
    This is done to avoid problems with filenames in the filesystem
'''

import sys
import os
import re
import shutil

# Check command-line parameters
fileStartArg = 1
if (sys.argv[1] == '-d'):
   destDir = sys.argv[2]
   fileStartArg = 3
else:
   destDir = "."
   
for i in range(fileStartArg, len(sys.argv)):

   # Open and read in the file
   handler = open(sys.argv[i])
   fileDirname = os.path.dirname(sys.argv[i])
   file = handler.read()

   # Get the name of the diagram
   tmatch = re.search(r'<b>(.*?)<\/b>',file, re.IGNORECASE)
   if tmatch:
       diagramName = tmatch.group(1)
       cleanName   = re.sub(r':&nbsp;','-',diagramName)
       cleanName   = re.sub(r'&nbsp;/&nbsp;','-',cleanName)
       cleanName   = re.sub(r'/','-',cleanName)
       cleanName   = re.sub(r'&nbsp;','_',cleanName)

   # Get the name of the picture file containing the diagram
   tmatch = re.search(r'<IMG SRC="(.*?)" USEMAP',file, re.IGNORECASE)
   if tmatch:
       diagramFile = fileDirname + "/" + tmatch.group(1)
       diagramName, diagramExt = os.path.splitext(diagramFile)
       
   # Now closes the file
   handler.close()

   # Builds the destination file name
   dest = destDir + "/" + cleanName + diagramExt
   print "Diagram:    ", cleanName
   print "  --> File: ", diagramFile, " copied into: ", dest     

   # Finally copies the file   
   shutil.copyfile(diagramFile, dest)
   
#
# ___oOo___


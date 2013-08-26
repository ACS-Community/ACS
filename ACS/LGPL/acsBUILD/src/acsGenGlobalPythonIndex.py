#!/usr/bin/env python
################################################################################################
# @(#) $Id: acsGenGlobalPythonIndex.py,v 1.1 2005/01/04 23:54:11 dfugate Exp $
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
This script is designed to create a global index file for all ACS Python documentation.
'''
################################################################################################
from os import listdir
from sys import argv
################################################################################################
#directory where the main pydoc is located
myDir = argv[1]

#list of all the HTML in the main directory
fileList = listdir(myDir)
#list of main packages and scripts only
goodList = []

#populate the good list with main packages and scripts only!
for file in fileList:
    if file.count('.')==1:
        goodList.append(file)

#organize it
goodList.sort()
goodList.reverse()

listLength = len(goodList)

elementNum = listLength/4 + 1

#print it out...
print '''
<!doctype html PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<html><head><title>ALMA Common Software</title>
</head><body bgcolor="#f0f0f8">

<table width="100%" cellspacing=0 cellpadding=2 border=0 summary="heading">
<tr bgcolor="#7799ee">
<td valign=bottom>&nbsp;<br>
<font color="#ffffff" face="helvetica, arial">&nbsp;<br><big><big><strong>ACS Global Python Documentation</strong></big></big></font></td>
<td align=right valign=bottom><font color="#ffffff" face="helvetica, arial">&nbsp;</font></td></tr></table>

<p><p>
<table width="100%" cellspacing=0 cellpadding=2 border=0 summary="section">
<tr bgcolor="#ee77aa">
<td colspan=3 valign=bottom>&nbsp;<br>
<font color="#ffffff" face="helvetica, arial"><big><strong>Packages, Modules, and Scripts</strong></big></font></td></tr>
    
<tr><td bgcolor="#ee77aa"><tt>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</tt></td><td>&nbsp;</td>
<td width="100%"><table width="100%" summary="list"><tr>
'''

print '''<!--There were originally ''', listLength, ''' directories and the elementNum is ''', elementNum, '''-->'''


#there will be four seperate columns
for i in range(0, 4):
    print '''  <td width="25%" valign=top>'''

    #each column is limited to elementNum elements
    for j in range(0, elementNum):
        if len(goodList)==0:
            break

        curFile = goodList.pop()

        print '''    <a href="''', curFile, '''">''', curFile.split('.')[0], '''</a><br>'''

    print '''  </td>'''


print '''
</tr></table></td></tr></table> <p>
</body></html>
'''
################################################################################################

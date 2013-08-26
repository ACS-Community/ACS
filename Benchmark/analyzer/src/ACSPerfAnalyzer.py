#!/usr/bin/env python
#------------------------------------------------------------------------------
# @(#) $Id: ACSPerfAnalyzer.py,v 1.5 2004/10/31 21:32:25 dfugate Exp $
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
#------------------------------------------------------------------------------
'''

TODO:
- all
'''
#------------------------------------------------------------------------------
__version__ = "$Id: ACSPerfAnalyzer.py,v 1.5 2004/10/31 21:32:25 dfugate Exp $"
#------------------------------------------------------------------------------
from sys import argv, exit
import          anydbm
import          re

from AcsutilPy.FindFile import findFile
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
def analyzeOutput(fileName, dbName):
    '''
    Analyze 
    '''
    #Open the file which contains one or more lines beginning with '#ACS PROFILER#'
    if findFile(fileName)[0]!="":
        fileName = findFile(fileName)[0]
    fileObj = open(fileName, 'r')

    #open the database
    if findFile(dbName)[0]!="":
        dbName=findFile(dbName)[0]
    db = anydbm.open(dbName, 'c')
    

    #Search through every line of the file looking for lines to analyze
    for line in fileObj:
        analyzeLine(line, db)

    #close the database and file
    db.close()
    fileObj.close()
#------------------------------------------------------------------------------
def analyzeLine(line, database):
    '''
    '''
    
    #pattern to search for
    rePattern = re.compile("#ACS PROFILER#")

    #if the the line describes a performance measurement...
    if re.match(rePattern, line) != None:
        #remove "#ACS PROFILER#"
        line = re.sub(rePattern, "", line)
        #strip out whitespace
        line = line.strip()
        #generate a list of name/value pairs
        goodStuff = line.split(", ")
        #remove extraneous whitespace in each name/value pair
        for i in range(0, len(goodStuff)):
            goodStuff[i]= goodStuff[i].strip()

        #temporary dictionary to be written out to the database
        tDict = {}

        #iterate through each and every name/value pair
        for stuff in goodStuff:
            (key, value) = stuff.split("=")

            #special cases used to define the unique key into the database
            if key=="msg":
                msg=value
            elif key=="date":
                date=value
            elif key=="ip":
                ip=value
        
            #in doing it this way we can store arbitrary performance data
            #not known ahead of time
            tDict[key]=value

        #should guarantee we have a unique key into the DB
        uniqueKey = ip + date + msg

        #add the stringified dictionary to the database
        database[uniqueKey]=str(tDict)
        
#------------------------------------------------------------------------------
if __name__=="__main__":
    DBNAME    = argv[1]  #name of the database
    INPUTFILENAME = argv[2]  #name of the input file
    analyzeOutput(INPUTFILENAME, DBNAME)
    

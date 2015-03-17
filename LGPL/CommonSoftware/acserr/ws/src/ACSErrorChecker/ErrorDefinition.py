#!/usr/bin/env python
################################################################################################
# @(#) $Id: ErrorDefinition.py,v 1.8 2011/10/28 14:39:18 hsommer Exp $
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

import xml.parsers.expat

class ErrorDefinition(object):
    """The errors definitions read by the xml file
    If the xml file is not a file of errors it contains a null list of errors
    
    The error has a name then a set of three values defining each type of errors
    (see the xml file for further details)
    Each error definition is composed by a dictionary of three values
        * code
        * sort description
        * description
    So the data struct is a list of dictionaries"""
    def __init__(self,xmlFile):
        self.file=xmlFile
        # Initialize the error
        
        # The name
        self.name=None 
        # The number of the error
        self.number=None 
        #The list of codes (each one is again a list of three fields) 
        self.codes=None 
        # The list of errors: each error is a list of 4 fields
        # name, shortDescription, description and a list of members 
        # that can be empty ([])
        # The list of members has key Members and value the list of members
        # i.e. {Members,[]}
        self.errors=None
        
        #Parse the xml
        params=parseFile(self.file)
        if params!=None and len(params)==4:
            self.name=params[0]
            self.number=params[1]
            self.errors=params[2]
            self.codes=params[3]
        
    def getErrors(self):
        return self.errors
        
    def getCodes(self):
        return self.codes
        
    def getName(self):
        return self.name
        
    def getNumber(self):
        return int(self.number)
        
    def getFile(self):
        return self.file
    
    def isValid(self):
        """The error is valid if the file was parsed OK 
        and the internal variables were filled"""
        return self.name!=None and self.number!=None
        
    def printError(self):
        """Nicely print the info about this error in the stdout (no html here!)
           """
        if not self.isValid():
            print "Invalid error (definition from ",self.file+")",
            return
        print "Error:",self.name
        print "Type:",self.number
        print "Defined in",self.file
        # Print the errors list
        if self.errors!=None:
            print "Errors:" 
            for error in self.errors:
                print "\tName:",error['name'],
		if error.has_key("shortDescription"):
                	print "\tShort description:",error["shortDescription"],
		else:
			print "\tShort description: N/A",
		if error.has_key("description"):
                	print '\tDescription:',error["description"]
		else:
			print '\tDescription: N/A'
        else:
            print "No errors defined"
        # Print the codes list
        if self.codes!=None:
            print "Codes:" 
            for code in self.codes:
                print "\tName:",code['name'],
		if error.has_key("shortDescription"):
                	print "\tShort description:",code["shortDescription"],
		else:
			 print "\tShort description: N/A",
		if error.has_key("description"):
                	print '\tDescription:',code["description"]
		else:
			print '\tDescription: N/A'
        else:
            print "No codes defined"

################################################
# The functions and variable to parse the file
################################################

#When the file is parsed, the following vars contain
# the values parsed. They are then stored in the
#object variables
theName=None
theErrors=None
theCodes=None
theNumber=None
#This global variable stores the name of the element we're parsing
# If the file is of the correct type it may be only Type or ErrCode
theTag=None
        
def parseFile(fileName):
    """Parse the file
    It set name to the name of the error
    and errors as a list of errors (of the same type)
    Return a list composed by
        * The name of the error
        * the type
        * the list of defined errors
        * the list of defined codes"""
    global theName,theNumber,theErrors,theCodes,theTag, theMembers
    theName=None
    theNumber=None
    theErrors=None
    theCodes=None
    theMembers=None
    theTag=None

    #Open the file to parse
    try:
        inF=open(fileName)
    except IOError, e:
        print "Unable to open",f
        print "Exception",e
        return 
        
    # Create the parser
    parser=xml.parsers.expat.ParserCreate()
    
    #set the handlers
    parser.StartElementHandler = start_element
    parser.EndElementHandler = end_element
    parser.CharacterDataHandler = char_data
    
    #Parse the file one line at a time (in order to avoid parsing an entire file 
    # if it is not an error definition file
    line=inF.readline()
    while line!="" and (theTag==None or theTag=="Type" or theTag=="ErrorCode" or theTag=="Code" or theTag=="Member"):
        line=inF.readline()
        if line==None or line=="":
            continue
        parser.Parse(line,line=="")
    
    #Close the input file
    inF.close
    
    #Stores the values in the list
    param=[]
    param.append(theName)
    param.append(theNumber)
    param.append(theErrors)
    param.append(theCodes)
    
    return param
    
        
def char_data(data):
    """Handler for parsing with expat"""

def start_element(name, attrs):
    """Handler for parsing with expat
    
    Type must be Type the first time then ErrCode otherwise
    the file is not of the right type"""
    global theName,theNumber,theErrors,theCodes,theTag, theMembers
    theTag=name
    if theTag=="Type":
        #The tag that defines the error
        theName=attrs["name"]
        theNumber=attrs["type"]
    if theTag=="ErrorCode":
        if theErrors==None:
            theErrors=[]
        theMembers=[]
        theErrors.append(attrs)
    if theTag=="Code":
        if theCodes==None:
            theCodes=[]
        theCodes.append(attrs)
    if theTag=="Member":
        theMembers.append(attrs)
        
def end_element(name):
    """Handler for parsing with expat"""
    global theName,theNumber,theErrors,theCodes,theTag, theMembers
    if name=="ErrorCode":
        error=theErrors[len(theErrors)-1]
        error['Members']=theMembers
        theMembers=[]

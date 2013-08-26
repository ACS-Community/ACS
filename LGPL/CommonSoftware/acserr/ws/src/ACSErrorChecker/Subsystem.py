#!/usr/bin/env python
################################################################################################
# @(#) $Id: Subsystem.py,v 1.18 2011/10/28 14:39:18 hsommer Exp $
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
# --------  ----------  ----------------------------------------------
# acaproni  2005/02/08  Created.
#------------------------------------------------------------------------------

import glob, os, string, re, xml.parsers.expat, sys, time, fpformat
from ErrorDefinition import *

class Subsystem:
	"""The object to scan a subsystem.
	   It assumes that the base directory contains 
	   all the sources of the subsystem
	   
	   name is the name of the subsystem
	   basedir is the base directory for the subsystem
	   min is the minimum allowed error number for this subsystem
	   max is the maximum allowed error number for this subsystem
	   ExaplesTestRange is the range for example and test errors
	   exclude is a list of directory trees to discard (CVS is a valid choice here)
	   include the directories to scan: if present only those folder
	           are scanned for the files (idl is a valide choice here)
	   """
	def __init__(self, name, basedir,min,max,exaplesAndTestsRange,exclude=["CVS"],include=None):
		# The maximum and the minimum allowed number for the errors of 
		# this subsystem
		self.min = min
		self.max = max
		#The name of this subsystem
		self.subsystemName=name
		#The base directory for the subsystem
		self.baseDir = basedir
		# The range for example and test errors
		self.testErrorRange=exaplesAndTestsRange
		# The times of start/end of this scan
		self.startTime=time.ctime()
		self.startFloatTime=time.time()
		self.endTime=self.startTime
		self.endFloatTime=self.startFloatTime
		print self.startTime+', scanning',self.subsystemName,'....'
		#The list of all the names of the files defining errors
		self.files=[]
		# The name of files and directory to exclude from the search path
		# (also the subdirectory are discarded)
		self.exclude=exclude
		# The directories to include in the search
		self.include=include
		# The list of number of duplicated errors
		self.duplicatedErrors=[]
		#The list of number out of the allowed range
		self.outOfRangeErrors=[]
		# The list of number for test and examples
		self.testAndExampleErrors = []
		# Print some infos
		print"Subsystem ",self.subsystemName,"with base dir",self.baseDir
		# Scans the directory of the subsystem
		self.scans(self.baseDir)
		print "Found",len(self.files),"candidates"
		self.candidates=len(self.files)
		#Parse the candidates to build the list of errors of the subsystems
		self.errors=self.parseXMLFiles(self.files)
		print "Found",len(self.files),"error definition files"
		print "Found",len(self.errors), "errors"
		#Order the errors
		self.orderErrors()
		# Check for duplicated errors
		self.checkSubsystemErrors()
		# Print info about duplicated errors
		if len(self.duplicatedErrors)==0:
			print "The subsystem contains no duplicated errors"
		else: 
			print "The subsystem contains the following duplicated error definitions:",
			print self.duplicatedErrors
		# Print info about errors out of range
		if len(self.outOfRangeErrors)==0:
			print "The subsystem contains no errors out of the allowed range"
		else: 
			print "The subsystem contains the following errors with an invalid number:",
			print self.outOfRangeErrors
		# Print info about the errors for examples and tests
		if len(self.testAndExampleErrors)==0:
			print "The subsystem does not containes definitions for tests and examples"
		else:
			print "The following errors are used for test and examples:",
			print self.testAndExampleErrors
		# Store the time
		self.endTime=time.ctime()
		self.endFloatTime=time.time()
		# Print the time
		print self.endTime+ ", check terminated"
		print fpformat.fix(self.endFloatTime-self.startFloatTime,3),"seconds requested to check this system"
		
	def excluded(self, name):
		"""Check if name is valid i.e. not part of the excluded"""
		if self.exclude==None or self.exclude==[]:
			return False
		for n in self.exclude:
			str = "/"+n
			sstr = name[len(name)-len(str):len(name)]
			if str == sstr: 
				return True
		return False
		
	def included(self,name):
		"""Check if name is a directory defined in include"""
		if self.include==None or self.include==[]:
			return True
		#Check if name is a directory
		if not os.path.isdir(name):
			return False
		names = string.split(name,'/')
		return self.include.count(names[len(names)-1])>0
			
	def isErrorDefFile(self,fileName):
		"""Check if fileName contains an error definition.
		It must be an xml file with the right format"""
		if os.path.isdir(fileName):
			return False
		if fileName[len(fileName)-4:len(fileName)]!=".xml":
			return False
		#The first line of an xml file contains something like <?xml...?>
		# So it doesn't work with included xml files....
		try:
			# Read the first line of the file
			inF=open(fileName)
			line=inF.readline().strip()
			inF.close()
			#Check if the line is of this kind: <?xml...?>
			return re.compile("<*\?xml.*\?>").match(line, 1)!=None
		except IOError, e:
			print "Unable to open",f
			print "Exception",e
			return False
		
	def scans(self,dir):
		"""Scans the base directory looking for the
		xml files defining erros"""
		for root, dirs, files in os.walk(dir):
			# Check if some of the files in the directory is an xml files to be added
			# to the list
			for f in files:
				if self.isErrorDefFile(os.path.join(root,f)) and self.included(root):
					self.files.append(os.path.join(root,f))
			#Checks if some of the directory must be excluded from the scan
			for d in dirs:
				if (self.excluded(os.path.join(root,d))):
					dirs.remove(d)
		
	def parseXMLFiles(self,xmlFiles):
		"""Parse all the xml files and bild a list of error definitions
		xmlFiles is the list of xml files to parse
		return a list of ErrorDefinition objects"""
		ret=[] # The errors
		errorFiles=[]# The new list of files
		for f in xmlFiles:
			err = ErrorDefinition(f)
			if err.isValid():
				ret.append(err)
				errorFiles.append(f)
		self.files=errorFiles
		return ret
		
	def orderErrors(self):
		"""Order the errors by their number"""
		newList=[]
		while len(self.errors)>0:
			# Extract the error or with the lower type
			# and insert it in the head of the new list
			pos=0
			min=self.errors[0].getNumber()
			for s in range(0,len(self.errors)):
				if self.errors[s].getNumber()<min:
					min = self.errors[s].getNumber()
					pos = s
			error=self.errors.pop(pos)
			newList.append(error)
		self.errors=newList
		
	def checkSubsystemErrors(self):
		"""Scan the erros of the subsystem and insert in the list all the numbers
		used by more than one error 
		
		It also check for errors whose number is out of the range [min,max]
		
		The errors with a number in the range allocated for test and examples 
		are always assumed OK
		
		Prerquisite: the errors are ordered by their number"""
		# Looks for duplicated error number by scanning all the errors
		# and comparing the actual number with the previous one
		for t in range(1,len(self.errors)):
			actNum = self.errors[t].getNumber()
			prevNum = self.errors[t-1].getNumber()
			if 	actNum == prevNum and \
				self.duplicatedErrors.count(actNum)==0 and \
				actNum<self.testErrorRange["Min"] and\
				actNum>self.testErrorRange["Max"]:
				self.duplicatedErrors.append(actNum)
		# Looks for errors out of the range for the subsystem
		# It also fills the list of the example and test errors
		for t in self.errors:
			if 	t.getNumber()>=self.testErrorRange["Min"] and \
				t.getNumber()<=self.testErrorRange["Max"]:
					self.testAndExampleErrors.append(t.getNumber())
			else:
				if t.getNumber()<self.min or t.getNumber()>self.max:
					if self.outOfRangeErrors.count(t.getNumber())==0:
						self.outOfRangeErrors.append(t.getNumber())
				
	def printErrors(self):
		"""Nicely print the info about this subsystem in 
		   the stdout (no html here!)
		   
		   Note: - this output is not defined as the html is.
		         - the user shoud read the out by himself to found errors
		   """
		print "Subsystem:",self.subsystemName
		print "Base dir:",self.baseDir
		# Print a list of duplicated errors
		for err in self.errors:
			if self.duplicatedErrors.count(err.getNumber())>0:
				print "The following error is defined more then once!"
            	print "\tType:",err.getNumber()
            	print "\tName: ",err.getName()
            	print "\tDefined in",err.getFile()
		# Print the errors
		for err in self.errors:
			err.printError()
	
	def getHTMLHeader(self,errorStr):
		"""Return the header of an HTML page (either the main page and the 
	    error description page)
		error is a string with the name of a specific error; if it is None 
		it means that the header is for the main page"""
		header='<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"\n"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">\n'
		header=header+'<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">\n'
		header=header+'<HEAD><TITLE>'
		if errorStr==None:
			header=header+'Error definitions for '+self.subsystemName
		else:
			header=header+'Details of '+self.subsystemName+'::'+errorStr
		header=header+'</TITLE></HEAD>\n'
		header=header+'<BODY>\n'
		return header
	
	def getHTMLFooter(self):
		"""Return the footer of an HTML page"""
		return '</BODY></HTML>\n'
	
	def insertFileContent(self,fileName):
		"""Return an HTML string with the content of the
		file with name fileName
		The string represents the verbatim content of the file"""
		str=''
		if os.access(fileName,os.R_OK):
			theFile = file(fileName,"r")
			lines = theFile.readlines()
			str=str+'<PRE>\n'
			for line in lines:
				line = line.replace('&','&amp;')
				line = line.replace('<','&lt;')
				line = line.replace('>','&gt;')
				str=str+line
			str=str+'\n</PRE>\n'
			theFile.close()
		else:
			str=str+'<P>Sorry, '+fileName+' is unreadable!'
		return str
		
	def generateHTMLPage(self,fileName):
		"""Generate the main HTML page for the errors
		The page is written into a file of the given name (filename)
		If fileName is None the page is written in the stdout
		For ach error described in this page there is a link to another HTML page
		with the detailed info about that error"""
		# count is used to generate the name of the sub pages describing 
		# the details of each error
		count = 0
		# The entire document is written into text
		text=self.getHTMLHeader(None);
		#Write the title of the page
		text=text+'<h1 align="center">Error definitions for '+self.subsystemName+'</h1>\n'
		text=text+'<p align="center">Generated scanning the basedir <I>'+self.baseDir+'</I><BR>\n'
		text=text+'<FONT size="-1">Generated at '+self.startTime+'</FONT></P>'
		text=text+'<HR size="3" width="80%" align="center">\n'
		# Print some info about the duplicated errors
		if len(self.duplicatedErrors)==0:
			text=text+"<P>The subsystem contains no duplicated errors</P>"
		else: 
			text=text+'<P>The subsystem contains the following duplicated error definitions:'
			text=text+'<UL>'
			for i in self.duplicatedErrors:
				text=text+'<LI>'+str(i)
				text=text+'<UL>' 
				for err in self.errors:
					if err.getNumber()==i:
						text=text+'<LI>'+err.getFile()
				text=text+'</UL>'
			text=text+'</UL></P>'
		# Print some info about the out of range type errors
		if len(self.outOfRangeErrors)==0:
			text=text+'<P>All the number of the errors are in the right range '
		else:
			text=text+'<P>The subsystem contains the following definition with an invalid number:'
			text=text+'<UL>'
			for i in self.outOfRangeErrors:
				text=text+'<LI>'+str(i)
				text=text+'<UL>' 
				for err in self.errors:
					if err.getNumber()==i:
						text=text+'<LI>'+err.getFile()
				text=text+'</UL>'
			text=text+'</UL></P>'
		#Print subsystem statistics
		text=text+'<h2>Statistics</h2>'
		text=text+'<TABLE>'
		text=text+'<TR><TD>'+str(self.candidates)+'</TD><TD>parsed candidate xml files</TD></TR>'
		text=text+'<TR><TD>'+str(len(self.errors))+'</TD><TD>error definition files found</TD></TR>'
		totErrors=0
		totCodes=0
		for err in self.errors:
			if err.getErrors()!=None:
				totErrors=totErrors+len(err.getErrors())
			if err.getCodes()!=None:
				totCodes=totCodes+len(err.getCodes())
		text=text+'<TR><TD>'+str(totErrors)+'</TD><TD>errors defined</TD></TR>'
		text=text+'<TR><TD>'+str(totCodes)+'</TD><TD>codes defined</TD></TR>'
		text=text+'</TABLE>'
		
		text=text+'<P>The range of the error numbers for this subsystem is <B>['+str(self.min)+','+str(self.max)+']</B></P>'
		text=text+'<P>The range for examples and tests is <B>['+str(self.testErrorRange["Min"])+','+str(self.testErrorRange["Max"])+']</B></P>'
		
		#Print all the errors (no examples and tests) into a table
		text=text+'<h2>Errors and codes defined</h2>\n'
		text=text+'<P>Type definitions appear in <B>bold</B><BR>\n'
		text=text+'Duplicated error types are printed in <FONT color="red">Red</FONT><BR>\n'
		text=text+'Errors with a number out of the allowed range appears <FONT color="red"><I><U>Red italic underline</U></I></FONT></P>\n'
		
		text=text+'<TABLE align="center" border="1">\n'
		#Go throw all the errors
		for err in self.errors:
			# Ignore the error if it is used for tests or examples
			if self.testAndExampleErrors.count(err.getNumber())>0:
				continue
			if self.duplicatedErrors.count(err.getNumber())>0 or self.outOfRangeErrors.count(err.getNumber())>0:
				# This is a duplicated error number or with an invalid number
				style ='<FONT color="red">'
				endStyle = '</FONT>'
				if self.outOfRangeErrors.count(err.getNumber())>0:
					# This error has a number out of range 
					style=style+'<I><U>'
					endStyle='</I></U>'+endStyle
			else:
				style=endStyle=''

			text=text+self.generateHTMLForError(err,style,endStyle,count,fileName)
			count=count+1
		text=text+'</TABLE>\n'
		
		# Print all the examples and test errors
		text=text+'<h3>Examples and tests</h3>'
		text=text+'<TABLE align="center" border="1">\n'
		for err in self.errors:
			# Print only the tests and the examples
			if self.testAndExampleErrors.count(err.getNumber())>0:
				text=text+self.generateHTMLForError(err,'','',count,fileName)
				count=count+1
		text=text+'</TABLE>\n'
		
		# Final infos
		text=text+'<BR><HR size="3" width="80%" align="center">\n'
		text=text+'<P><FONT size="-1">Subsystem scanned in '+fpformat.fix(self.endFloatTime-self.startFloatTime,3)+' seconds.</FONT></P>'
		
		#Close the document
		text=text+self.getHTMLFooter()
		
		#Write the page on disk
		self.writeHTMLOnFile(text, fileName)

	def generateHTMLForErrorMember(self,error,fontStyle,endFontStyle):
		"""Return an HTML string of the Members of the error (if any)"""
		
		members=error['Members']
		htmlText='<TABLE>'
		htmlText+='<TR><TD><I>Name</I></TD><TD><I>Type</I></TD><TD><I>Description</I></TD></TR>'
		for m in error['Members']:
			htmlText+='<TR>\n';
			if m.has_key('name'):
				htmlText+='<TD>'+m['name']+'</TD>\n'
			else:
				htmlText+="<TD> </TD>\n"
			if m.has_key('type'):
				htmlText+='<TD>'+m['type']+'</TD>\n'
			else:
				html+="<TD> </TD>\n"
			if m.has_key('description'):
				htmlText+='<TD>'+m['description']+'</TD>\n'
			else:
				htmlText+="<TD> </TD>\n"
			htmlText+='</TR>\n'
		htmlText+='</TD></TABLE>'
		return htmlText
	
	def generateHTMLForError(self,error,fontStyle,endFontStyle,offset,fileName):
		"""Return an html string for the passed error
		The html generated is a table of errors
		
		error is the error to scan
		fontstyle and endFontStyle are used to color the wrong errors
		offset is used to generate the name of the sub pages (that describe
			   the details of each error)"""
		htmlText='<TR valign="top">\n'
		errors=error.getErrors()
		codes=error.getCodes()
		# Print the error type and number
		rowSpan=1
		if errors!=None:
			rowSpan = rowSpan + len(errors)+1
		if codes!=None:
			rowSpan = rowSpan + len(codes)+1
		# Build the name of the HTML subpage changing the name before the .html
		# extension
		# First find the position of '.html'
		pos = fileName.rfind('html')
		# Change the name replacing the _d<n>.html to original extension .html
		detailFileName = fileName[0:pos-1]+'_d'+str(offset)+'.html'
		# The link of the detailed error HTML page must be relative
		# so we check if a '/ is present in the detailFileName to create the
		# linkFileName variable
		pos = detailFileName.rfind('/')
		if pos!=-1:
			# The detail file name is relative
			linkFileName=detailFileName[pos+1:]
		else:
			linkFileName=detailFileName
		self.generateErrorDetailsHTMLPage(error, detailFileName)
		htmlText=htmlText+'<TD rowspan="'+str(rowSpan)+'">'+fontStyle+'<B>'+str(error.getNumber())+'</B>'+endFontStyle+'</TD>\n'
		htmlText=htmlText+'<TD>'+fontStyle+'<B>'
		htmlText=htmlText+'<A href="'+linkFileName+'">'+error.getName()+'</A>'
		htmlText=htmlText+'</B>'+endFontStyle+'</TD>\n'
		htmlText=htmlText+'<TD colspan="2">'+fontStyle+'<B>'+error.getFile()+'</B>'+endFontStyle+'</TD>\n'
		htmlText=htmlText+'</TR>\n'
		if errors!=None:
			htmlText=htmlText+'<TR><TD rowspan="'+str(len(errors)+1)+'">'+fontStyle+'<I>Errors</I>'+endFontStyle+'</TD></TR>'
			for t in errors:
				# Print the defined ErrorCode
				htmlText=htmlText+'<TR>\n'
				htmlText=htmlText+'<TD><A href="'+linkFileName+'#'+t['name']+'">'+fontStyle+t['name']+endFontStyle+'</A></TD>\n'
				if t.has_key("shortDescription"):
					htmlText=htmlText+'<TD>'+fontStyle+t["shortDescription"]+endFontStyle+'</TD>\n'
				else:
					htmlText=htmlText+'<TD>'+fontStyle+"N/A"+endFontStyle+'</TD>\n'
				htmlText=htmlText+'</TR>\n'
					
		if codes!=None:
			htmlText=htmlText+'<TR><TD rowspan="'+str(len(codes)+1)+'">'+fontStyle+'<I>Codes</I>'+endFontStyle+'</TD></TR>'
			for t in codes:
				# Print the defined ErrorCode
				htmlText=htmlText+'<TR>\n'
				htmlText=htmlText+'<TD><A href="'+linkFileName+'#'+t['name']+'">'+fontStyle+t['name']+endFontStyle+'</A></TD>\n'
				if t.has_key("shortDescription"):
					htmlText=htmlText+'<TD>'+fontStyle+t["shortDescription"]+endFontStyle+'</TD>\n'
				else:
                                        htmlText=htmlText+'<TD>'+fontStyle+"N/A"+endFontStyle+'</TD>\n'
				htmlText=htmlText+'</TR>\n'
		return htmlText
		
	def generateHTMLForDetailedError(self,error,fontStyle,endFontStyle):
		"""Return an html string for the passed error
		The html is a row of the table <TR><TD>....</TD><TR>"""
		errors = error.getErrors()
		codes = error.getCodes()
		
		htmlText='<TABLE border="1">\n'
		htmlText=htmlText+'<TR><TD><I>Error name:</I></TD><TD><B>'+error.getName()+'</B></TD></TR>'
		htmlText=htmlText+'<TR><TD><I>Error code:</I></TD><TD><B> '+str(error.getNumber())+'</B></TD></TR>'
		htmlText=htmlText+'<TR><TD><I>The error is defined in:</I></TD><TD><B> <A href="#XML">'+error.getFile()+'</A></B></TD></TR>'
		htmlText=htmlText+'<TR><TD colspan="2"><B>IDL</B></TD></TR>'
		htmlText=htmlText+'<TR><TD><I>File name:</I></TD><TD>'+error.getName()+'.idl</TD></TR>'
		htmlText=htmlText+'<TR><TD><I>Module:</I></TD><TD>'+error.getName()+'</TD></TR>'
		htmlText=htmlText+'<TR><TD colspan="2"><B>C++</B></TD></TR>'
		htmlText=htmlText+'<TR><TD><I>Include file name:</I></TD><TD>#include &lt;'+error.getName()+'.h&gt;</TD></TR>'
		htmlText=htmlText+'<TR><TD><I>Namespace:</I></TD><TD>using namespace '+error.getName()+';</TD></TR>'
		htmlText=htmlText+'<TR><TD><I>Exception for type:</I></TD><TD>'+error.getName()+'::'+error.getName()+'ExImpl</TD></TR>'
		htmlText=htmlText+'<TR><TD><I>Library file name:</I></TD><TD>lib'+error.getName()+'.so<BR>lib'+error.getName()+'.a</TD></TR>'
		htmlText=htmlText+'<TR><TD colspan="2"><B>Python</B></TD></TR>'
		htmlText=htmlText+'<TR><TD><I>Python imports:</I></TD><TD>import '+error.getName()+'Impl</TD></TR>'
		htmlText=htmlText+'<TR><TD colspan="2"><B>Java</B></TD></TR>'
		htmlText=htmlText+'<TR><TD><I>Jar file name:</I></TD><TD>'+error.getName()+'.jar</TD></TR>'
		htmlText=htmlText+'<TR><TD><I>Packages:</I></TD><TD>import alma.'+error.getName()+'.*;<BR>import alma.'+error.getName()+'.wrapper.*;</TD></TR>'
		htmlText=htmlText+'<TR><TD><I>Exception for type:</I></TD><TD>alma.'+error.getName()+'.'+error.getName()+'Ex</TD></TR>'
		htmlText=htmlText+'<TR><TD><I>Exception for type (wrapper):</I></TD><TD>alma.'+error.getName()+'.wrapper.AcsJ'+error.getName()+'Ex</TD></TR>'
		htmlText=htmlText+'</TABLE><BR>\n'
		
		# Print the error type and number
		rowSpan=1
		if errors!=None:
			rowSpan = rowSpan + len(errors) + 1
			for t in errors:
				if t.has_key('Members'):
					if len(t['Members'])>0:
						rowSpan = rowSpan +len(t['Members']) + 1
		if codes!=None:
			rowSpan = rowSpan + len(codes) + 1
		
		if errors!=None:
			htmlText=htmlText+'<H2>Errors</H2>'
			for t in errors:
				htmlText=htmlText+'<h3><A name="'+t['name']+'">'+fontStyle+t['name']+endFontStyle+'</A></h3>\n'
				htmlText=htmlText+'<TABLE border="1">\n'
				htmlText=htmlText+'<TR><TD><I>Short description</I></TD>'
				if t.has_key("shortDescription"):
					htmlText=htmlText+'<TD>'+fontStyle+t["shortDescription"]+endFontStyle+'</TD>\n'
				else:
					htmlText=htmlText+'<TD>'+fontStyle+"N/A"+endFontStyle+'</TD>\n'
				htmlText=htmlText+'</TR>'
				htmlText=htmlText+'<TR><TD><I>Description</I></TD>'
				if t.has_key("description"):
					htmlText=htmlText+'<TD>'+fontStyle+t["description"]+endFontStyle+'</TD>\n'
				else:
					htmlText=htmlText+'<TD>'+fontStyle+"N/A"+endFontStyle+'</TD>\n'
				
				if t.has_key('Members') and len(t['Members'])>0:
					htmlText=htmlText+'<TR><TD><I>Members</I></TD><TD>'					
					htmlText=htmlText+self.generateHTMLForErrorMember(t,fontStyle,endFontStyle)
					htmlText=htmlText+'</TD></TR>'
				htmlText=htmlText+'<TR><TD colspan="2"><B>IDL</B></TD></TR>'
				htmlText=htmlText+'<TR><TD><I>File name:</I></TD><TD>'+error.getName()+'.idl</TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Module:</I></TD><TD>'+error.getName()+'</TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Error code:</I></TD><TD>'+t['name']+'</TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Exception:</I></TD><TD>'+t['name']+'Ex</TD></TR>'
				htmlText=htmlText+'<TR><TD colspan="2"><B>C++</B></TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Include file name:</I></TD><TD>#include &lt;'+error.getName()+'.h&gt;</TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Namespace:</I></TD><TD>using namespace '+error.getName()+';</TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Exception for error:</I></TD><TD>'+error.getName()+'::'+t['name']+'ExImpl</TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Completion:</I></TD><TD>'+error.getName()+'::'+t['name']+'Completion</TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Library file name:</I></TD><TD>lib'+error.getName()+'.so<BR>lib'+error.getName()+'.a</TD></TR>'
				htmlText=htmlText+'<TR><TD colspan="2"><B>Python</B></TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Imports:</I></TD><TD>import '+error.getName()+'Impl</TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Raise a local exception:</I></TD><TD>raise '+error.getName()+'Impl.'+t['name']+'ExImpl()</TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Catch exception:</I></TD><TD>except '+error.getName()+'Impl.'+t['name']+'Ex, e:</TD></TR>'
				htmlText=htmlText+'<TR><TD colspan="2"><B>Java</B></TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Jar file name:</I></TD><TD>'+error.getName()+'.jar</TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Packages:</I></TD><TD>import alma.'+error.getName()+'.*;<BR>import alma.'+error.getName()+'.wrapper.*;</TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Exception for error:</I></TD><TD>alma.'+error.getName()+'.'+t['name']+'Ex</TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Exception for error (wrapper):</I></TD><TD>alma.'+error.getName()+'.wrapper.AcsJ'+t['name']+'Ex</TD></TR>'
				htmlText=htmlText+'</TABLE><BR>\n'
				
				
					
		htmlText=htmlText+'<H2>Codes</H2>'
		if codes!=None:
			for t in codes:
				# Print the defined ErrorCode
				htmlText=htmlText+'<H3><A name="'+t['name']+'">'+fontStyle+t['name']+endFontStyle+'</A></H3>\n'
				htmlText=htmlText+'<TABLE border="1">\n'
				htmlText=htmlText+'<TR><TD><I>Short description</I></TD>'
				if t.has_key("shortDescription"):
					htmlText=htmlText+'<TD>'+fontStyle+t["shortDescription"]+endFontStyle+'</TD>\n'
				else:
                                        htmlText=htmlText+'<TD>'+fontStyle+"N/A"+endFontStyle+'</TD>\n'
				htmlText=htmlText+'</TR>'
				htmlText=htmlText+'<TR><TD><I>Description</I></TD>'
				if t.has_key("description"):
					htmlText=htmlText+'<TD>'+fontStyle+t["description"]+endFontStyle+'</TD>\n'
				else:
                                        htmlText=htmlText+'<TD>'+fontStyle+"N/A"+endFontStyle+'</TD>\n'
				htmlText=htmlText+'</TR>\n'
				htmlText=htmlText+'<TR><TD colspan="2"><B>IDL</B></TD></TR>'
				htmlText=htmlText+'<TR><TD><I>File name:</I></TD><TD>'+error.getName()+'.idl</TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Module:</I></TD><TD>'+error.getName()+'</TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Error code:</I></TD><TD>'+t['name']+'</TD></TR>'
				htmlText=htmlText+'<TR><TD colspan="2"><B>C++</B></TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Include file name:</I></TD><TD>include &lt;'+error.getName()+'.h&gt;</TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Namespace:</I></TD><TD>using namespace '+error.getName()+';</TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Completion:</I></TD><TD>'+error.getName()+'::'+t['name']+'Completion</TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Library file name:</I></TD><TD>lib'+error.getName()+'.so<BR>lib'+error.getName()+'.a</TD></TR>'
				htmlText=htmlText+'<TR><TD colspan="2"><B>Python</B></TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Imports:</I></TD><TD>import '+error.getName()+'Impl</TD></TR>'
				htmlText=htmlText+'<TR><TD colspan="2"><B>Java</B></TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Jar file name:</I></TD><TD>'+error.getName()+'.jar</TD></TR>'
				htmlText=htmlText+'<TR><TD><I>Packages:</I></TD><TD>import alma.'+error.getName()+'.*;<BR>import alma.'+error.getName()+'.wrapper.*;</TD></TR>'
			htmlText=htmlText+'</TABLE>\n'
		else:
			htmlText=htmlText+'<P>No codes defined</>'
		# Insert the XML
		htmlText=htmlText+'<H2><A name="XML">XML</A></H2>\n'
		htmlText=htmlText+self.insertFileContent(error.getFile())
		return htmlText
	
	def generateErrorDetailsHTMLPage(self,error,fileName):
		"""Generate the HTML with the detail of a single error"""
		page=self.getHTMLHeader(error.getName())
		#Write the title of the page
		page=page+'<h1 align="center">Error of '+self.subsystemName+'::'+error.getName()+'</h1>\n'
		page=page+'<p align="center">Generated scanning the basedir <I>'+self.baseDir+'</I><BR>\n'
		page=page+'<FONT size="-1">Generated at '+self.startTime+'</FONT></P>'
		page=page+'<HR size="3" width="80%" align="center">\n'
		
		#Generate the table with the error details
		page=page+self.generateHTMLForDetailedError(error, '', '')
		
		page=page+'<H2>Info and errors</H2>'
		
		if self.testAndExampleErrors.count(error.getNumber())>0:
			page=page+'<P>This error is used for test/examples</P>'
		else:
			if self.outOfRangeErrors.count(error.getNumber())>0:
				page=page+'<P><FONT color="red">The error number is out of the allowed range for this subsystem</FONT></P>'
			else:
				page=page+'<P>The number is in the corrrect range</P>'
			if self.duplicatedErrors.count(error.getNumber())>0:
				page=page+'<P><FONT color="red">There is another error in this subsystem with the same number</FONT></P>'
			else:
				page=page+'<P>The error is not duplicated</P>'
		
		# Final infos
		page=page+'<BR><HR size="3" width="80%" align="center">\n'
		page=page+'<P><FONT size="-1">Subsystem scanned in '+fpformat.fix(self.endFloatTime-self.startFloatTime,3)+' seconds.</FONT></P>'
		
		page=page+self.getHTMLFooter()
		self.writeHTMLOnFile(page, fileName)

	def writeHTMLOnFile(self,page,fileName):
		"""page if an string with the HTML page to write in the file
		fileName is the name of the file
		If fileName is None the page is written in the stdout"""
		if fileName==None:
			print page # To stdout
		else:
			try:
				outF=open(fileName,"w+")
				outF.write(page)
				outF.flush()
				outF.close
			except IOError, e:
				print "Unable to open ",fileName, "for output"
				print "Exception",e
        		return
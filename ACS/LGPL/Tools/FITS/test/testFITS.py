#!/usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2002 
# (c) European Southern Observatory, 2002
# Copyright by ESO (in the framework of the ALMA collaboration)
# and Cosylab 2002, All rights reserved
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307  USA
#
# @(#) $Id: testFITS.py,v 1.3 2008/05/30 20:25:21 agrimstrup Exp $
###############################################################################
'''
Tests the Python FITS libraries.
'''
#A. Caproni 15/01/2004
import string
import sys

#Imports for FITS
from numpy import *
import pyfits

"""This program is intended to check the installation and function of PyFITS"""

def Usage():
	print Args[0],'<img>'

def ExecuteCmd(cmd):
	"""Execute the cmd command on the image
	Recognized commands are:
	read key
	delete key
	readall
	update key = value / comment
	save filename
	help
	?
	quit

	read: search and prints on stdout the value and comment of key
	readall: prints all the keys
	update: change the value of key or insert a new key if the key doesn't exist
	save: save the changes on the file filename
	quit: terminate
	help: print a short help with all the possible commands
	?: same as help
	Commands are case insensitive"""
	# keep a copy of the original command
	originalCmd=cmd
	#Clean the string
	cmd=string.strip(cmd)
	cmd=string.rstrip(cmd,"\n")
	cmd=string.upper(cmd) # case insensitive
	words=string.split(cmd)
	if words[0]=="READ":
		if len(words)==1:
			print "> ? syntax error ?"
			return 1
		if prihdr.has_key(words[1]):
			print words[1],"=",prihdr[words[1]]
		else:
			print ">",words[1],"not found"
	elif words[0]=="QUIT":
		return 0
	elif words[0]=="READALL":
		#Print all the keys
		for key in keys:
			print ">",key
	elif words[0]=="SAVE":
		#In this case words[1] contains the name of the new file to save
		if len(words)==2:
			# Use originalCmd because now cmd is uppercase
			originalWords=string.split(originalCmd)
			fimg.writeto(originalWords[1])
		else:
			print "> ? syntax error ?"
			return 1
	elif words[0]=="DELETE":
		if len(words)==1:
			print "> ? syntax error ?"
			return 1
		if prihdr.has_key(words[1]):
			del prihdr[words[1]]
		else:
			print ">",words[1],"not found"
			return 1
	elif words[0]=="UPDATE":
		myCmd=string.replace(cmd,"UPDATE"," ",1)
		myCmd=string.strip(myCmd)
		keyWords = string.split(myCmd,"=")
		if len(keyWords)<2:
			print "> ? syntax error ?"
			return 1
		else:
			keyName=string.strip(keyWords[0])
			pos=string.find(myCmd,"=")
			if pos==-1:
				print "> ? syntax error ?"
				return 1
			cmdNoName=myCmd[pos+1:len(myCmd)]
			cmdNoName=string.strip(cmdNoName)
			pos=string.find(cmdNoName,"/")
			if pos==-1:
				value=cmdNoName
				comment=""
				# No comment
			else:
				#read the value
				value=string.strip(cmdNoName[0:pos])
				#read the comment
				comment=cmdNoName[pos+1:len(cmdNoName)]
				comment=string.strip(comment)
			print "keyName ["+keyName+"]"
			print "value ["+value+"]"
			print "comment ["+comment+"]"
			prihdr.update(keyName,value,comment=comment)
			return 1
	elif words[0]=="HELP" or words[0]=="?":
		print "Available commands:"
		print "\tread key"
		print "\treadall"
		print "\tupdate key = value/comment"
		print "\tdelete key"
		print "\tsave filename"
		print "\thelp"
		print "\t?"
		print "\tQuit"
		print "EOF to terminate"
	else:
		print "> ? Unrecognized command:",words[0],"?"
		print "> use help or ? for help"
	return 1

Args = sys.argv

#Check if the name of the image is in the command line
if len(Args)<2:
	Usage()
	sys.exit(-1)

#Open the fits
try:
	#fimg = pyfits.open(Args[1],"copyonwrite",1)
	fimg=pyfits.open(Args[1])
except:
	print "Error opening",Args[1]
	sys.exit(-1)

#Print the name of the image
print "fimg:",fimg

#Print some general info
fimg.info()

#Print (friendly) keys, values and comments of the header
prihdr=fimg[0].header;
print "ITEMS:"
keys=prihdr.items()

#Print info about the image
print "Reading image data"
imgdata=fimg[1].data
print "Image is:",imgdata.shape

# Read from stdin and execute commands until EOF
done=1
while done:
	done=0
	print "> ",
	line = sys.stdin.readline()
	if not line: 
		break
	else:
		done=ExecuteCmd(line)

#Close the FITS
fimg.close()

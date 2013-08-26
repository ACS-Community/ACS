#!/usr/bin/env python

"""Create the empty file for the recovery test

   USAGE: SetupTest -c|-d <file name>

   -c: create the file <file name> 
   -d: delete the file <file name> (cleanup)

   Here empty means a file with no data but with the right format
   (the separator and a new line)"""
import os,sys,string

if len(sys.argv)!=3:
	print 'Sysntax error: wrong args number'
	sys.exit(-1)

#Read the params in the command line 
Opt=sys.argv[1]
FileName=sys.argv[2]

if Opt=='-c':
	# Open and write the file
	f=open(FileName,"w")
	f.write(chr(1))
	f.write('\n')

	f.write('Ale')
	f.write(chr(1))
	f.write('Ciao\n')
 	f.close()
elif Opt=='-d':
	# delete the file
	if os.access(FileName,os.F_OK):
		os.remove(FileName)
else:
	# Syntax error
	print 'Sintax error: invalid options',Opt
	sys.exit(-1)


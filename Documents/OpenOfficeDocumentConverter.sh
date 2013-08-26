#!/bin/bash

#
# This is a very crude script to drive the conversion
# of documents using Open Office.
# It should be made nicer, more protectec from errors, 
# less verbose and, probably, re-written in Python
# to avoid using the bash, that is not nicely portable.
#
inputFile=$1
outputFile=$2

echo "Converting $1 in $2 using Open Office"

# Try to autodetect OOFFICE and OOOPYTHON.
OOFFICE=`ls /usr/bin/openoffice.org3 /opt/openoffice.org3/program/soffice | head -n 1`
OOOPYTHON=`ls /opt/openoffice.org*/program/python /usr/bin/python | head -n 1`

if [ ! -x "$OOFFICE" ]
then
 echo "Could not auto-detect OpenOffice.org binary"
 exit
fi

if [ ! -x "$OOOPYTHON" ]
then
 echo "Could not auto-detect OpenOffice.org Python"
 exit
fi

# Reference: http://wiki.services.openoffice.org/wiki/Using_Python_on_Linux
# If you use the OpenOffice.org that comes with Fedora or Ubuntu, uncomment the following line:
# export PYTHONPATH="/usr/lib/openoffice.org/program" 

# If you want to simulate for testing that there is no X server, uncomment the next line.
#unset DISPLAY

# Kill any running OpenOffice.org processes.
killall -u `whoami` -q soffice

# The original converter come from here:
# wget http://www.artofsolving.com/files/DocumentConverter.py

# Start OpenOffice.org in listening mode on TCP port 8100.
$OOFFICE "-accept=socket,host=localhost,port=8100;urp;StarOffice.ServiceManager" -norestore -nofirststartwizard -nologo -headless  &

# Wait a few seconds to be sure it has started.
sleep 5s

# Convert as many documents as you want serially (but not concurrently).
# Substitute whichever documents you wish.
$OOOPYTHON OpenOfficeDocumentConverter.py $inputFile $outputFile

# Close OpenOffice.org.
killall -u `whoami` soffice

echo "Done PDF conversion from Open Office"

#__oOo__


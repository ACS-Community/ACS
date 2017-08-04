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
# @(#) $Id: acspyTestLogging.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $

'''
Demonstration of how to use the acspyPySimpleClient class
to access an ACS DO from a Python program
'''

__version__ = "$Id: acspyTestLogging.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

from Acspy.Common.Log import getLogger
import sys
import time

print ' '
print '============= Basic tests ============'
print ' '

# Get a Logger instance
logger = getLogger()

# Very basic examples of logs
logger.logInfo('Test INFO message')

logger.logTrace('Test TRACE message')

logger.logDebug('Test DEBUG message')

logger.logWarning('Test WARNING message')

logger.logAtLevel(7,'Test ERROR message')

logger.logError('Test ERROR message')

logger.logAlert('Test ALERT message')

logger.logCritical('Test CRITICAL message')

logger.logEmergency('Test EMERGENCY message')

logger.logNotice('Test NOTICE message')

print '============= End basic tests ============'
print ' '


print '============= Complex tests ============'
#
# More complex tests
# Ignore if you are just learning how to log from Python
#

#
# Defines a function that performs some logs
#
def aFunction(aLogger):
    print '== Logging INFO message in "aFunction"'
    aLogger.logInfo("Test INFO message from a function")

aFunction(logger)

# TODO: logError and logXML
print '== Test logError'
import ACSErr

tempET = ACSErr.ErrorTrace(str("file"),        #string file;
                           int(43),        #long lineNum;
                           str("routine"),     #string routine;
                           str("host"),        #string host;
                           str("process"),     #string process;
                           str("thread"),      #string thread;
                           long(23L),        #unsigned long long timeStamp;
                           str("sourceObject"), #string souceObject;
                           long(1),          #ACSErr::ACSErrType errorType;
                           long(2),       #ACSErr::ErrorCode errorCode;
                           ACSErr.Alert,    #ACSErr::Severity severity;
                           "no description", #string description
                           [],       #NameValueSeq data;
                           [])  #sequence<ErrorTrace, 1> previousError;
logger.logErrorTrace(tempET)

print "== The end __oOo__"

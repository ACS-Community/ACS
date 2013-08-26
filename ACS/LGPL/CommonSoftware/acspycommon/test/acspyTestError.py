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
# @(#) $Id: acspyTestError.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
###############################################################################
"""
Tests the Python Error system.
"""
###############################################################################
import ACSErrTypePythonNative
import ACSErrTypePythonNativeImpl
import ACSErr
import ACSLog
from Acspy.Common.Err import pyExceptionToCORBA
from Acspy.Common.Log import getLogger

###############################################################################
def fakeCORBAMethod():
    '''
    Simulates the testing of a fake CORBA method
    This function:
    - creates a new native local exception and throws it
    - catches it as native exception
    - converts it to an ACS exception using pyExceptionToCORBA() function
    - throws it again
    - catches it as a CORBA exception
    - converts it back into the helper class WITHOUT adding new error info
    - rethrows the new local exception
    - catches it as a CORBA exception
    - converts it back into the helper class AND adds new error info
    - throws the exception again which should have two error traces
    '''
    print "--fakeCORBAMethod1-------------------------------------------------"
    try:
        try:
            raise Exception("fake python exception")
        except Exception, ex:
            print "Raising the local exception..."
            raise pyExceptionToCORBA(ex)
        
    #make sure we can catch the real CORBA exception
    except ACSErrTypePythonNative.PythonEx, e:
        #convert it back into the helper class w/o adding error information
        helperException = ACSErrTypePythonNativeImpl.PythonExImpl(exception=e, create=0)
        #print to stdout...only one error trace should be seen
        helperException.Print()
    
    print "--fakeCORBAMethod2-------------------------------------------------"
    try:
        #reraise a local ACS exception
        raise helperException
    except ACSErrTypePythonNative.PythonEx, e:
        #make sure we can catch the real CORBA exception
        helperException = ACSErrTypePythonNativeImpl.PythonExImpl(exception=e)
        #Printing to stdout AGAIN...should see TWO errorTraces this time around
        helperException.Print()

    #finally raise the exception out of the pseudo CORBA method
    raise helperException

###############################################################################
def fakeClientFunction():
    '''
    Invokes a fake CORBA method which raises a fake exception.
    This function:
    - invokes a fake CORBA method which raises a fake CORBA exception
    - catches the CORBA exception
    - converts it back into the helper class AND adds new error info
    - throws the exception again which should have three error traces
    '''
    print "--fakeClientFunction1-------------------------------------------------"
    try:
        fakeCORBAMethod()
    except ACSErrTypePythonNative.PythonEx, e:
        print "--fakeClientFunction2-------------------------------------------------"
        helperException = ACSErrTypePythonNativeImpl.PythonExImpl(exception=e)
        #Printing to stdout...should see three errorTraces
        helperException.Print()
    raise helperException

###############################################################################
def fakeCORBAMethodNew():
    '''
    Simulates the testing of a fake CORBA method
    This function:
    - creates a new native local exception and throws it
    - catches it as native exception
    - creates a new ACS exception out of it, retaining all its information
    - throws it again
    - catches it as a CORBA exception
    - converts it back into the helper class WITHOUT adding new error info
    - rethrows the new local exception
    - catches it as a CORBA exception
    - converts it back into the helper class AND adds new error info
    - throws the exception again which should have two error traces
    '''
    print "--fakeCORBAMethodNew1-------------------------------------------------"
    try:
        try:
            raise Exception("fake python exception")
        except Exception, ex:
            print "Raising the local exception..."
            raise ACSErrTypePythonNativeImpl.PythonExImpl(exception=ex)
        
    #make sure we can catch the real CORBA exception
    except ACSErrTypePythonNative.PythonEx, e:
        #convert it back into the helper class w/o adding error information
        helperException = ACSErrTypePythonNativeImpl.PythonExImpl(exception=e, create=0)
        #print to stdout...only one error trace should be seen
        helperException.Print()
    
    print "--fakeCORBAMethodNew2-------------------------------------------------"
    try:
        #reraise a local ACS exception
        raise helperException
    except ACSErrTypePythonNative.PythonEx, e:
        #make sure we can catch the real CORBA exception
        helperException = ACSErrTypePythonNativeImpl.PythonExImpl(exception=e)
        #Printing to stdout AGAIN...should see TWO errorTraces this time around
        helperException.Print()

    #finally raise the exception out of the pseudo CORBA method
    raise helperException


###############################################################################
if __name__ == "__main__":
    logger = getLogger('Error Test')
    print "--main1-------------------------------------------------"
    try:
        fakeClientFunction()
    except ACSErrTypePythonNative.PythonEx, e:
        print "--main2-------------------------------------------------"
        helperException = ACSErrTypePythonNativeImpl.PythonExImpl(exception=e)
        #should be four error traces at this point
        helperException.Print()
    
    print "--main2-------------------------------------------------"
    print "Testing all public methods"
    print ""
    print "Grep me out", helperException.getErrorTrace()
    print "Grep me out", helperException.getNext()
    helperException.log(logger)
    helperException.log(logger, ACSLog.ACS_LOG_DEBUG)
    print "Grep me out", helperException.isOK()
    helperException.addData("name", "value")
    print "getData('no data set'):", helperException.getData("no data set")
    print "getData('name'):", helperException.getData("name")
    print "Grep me out", helperException.getDescription()
    print "Grep me out", helperException.getFileName()
    print "Grep me out", helperException.getLineNumber()
    print "Grep me out", helperException.getRoutine()
    print "Grep me out", helperException.getHostName()
    print "Grep me out", helperException.getProcess()
    print "Grep me out", helperException.getThread()
    print "Grep me out", helperException.getTimeStamp()
    print "Grep me out", helperException.getErrorCode()
    print "Grep me out", helperException.getErrorType()
    print "Grep me out", helperException.getSeverity()
    helperException.setTimeStamp(23L)
    helperException.setFileName("noFileName")
    helperException.setLineNumber(1L)
    helperException.setError(0, 0)
    helperException.setSeverity(ACSErr.Error)
        
    print "--mainNew1-------------------------------------------------"
    try:
        fakeCORBAMethodNew()
    except ACSErrTypePythonNative.PythonEx, e:
        print "--mainNew2-------------------------------------------------"
        helperException = ACSErrTypePythonNativeImpl.PythonExImpl(exception=e)
        #should be four error traces at this point
        helperException.Print()



print "The end __oOo__"

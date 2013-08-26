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
# @(#) $Id: acspyTestBaci.py,v 1.3 2007/10/04 21:56:15 agrimstrup Exp $

#------------------------------------------------------------------------------
'''
'''
#------------------------------------------------------------------------------
from Acspy.Clients.SimpleClient import PySimpleClient # Import the acspy.PySimpleClient class
import ACS                                            # Import the Python CORBA stubs for BACI
from   Acspy.Common.Callbacks import CBstring
from   Acspy.Common.Callbacks import CBstringSeq
from   Acspy.Common.Callbacks import CBdouble
from   Acspy.Common.Callbacks import CBdoubleSeq
from   Acspy.Common.Callbacks import CBlong
from   Acspy.Common.Callbacks import CBlongSeq
from   Acspy.Common.Callbacks import CBlongLong
from   Acspy.Common.Callbacks import CBuLongLong
from   Acspy.Common.Callbacks import CBpattern
from   Acspy.Common.Callbacks import CBvoid
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
def testBasicROProperty(propRef, cbRef):
    '''
    Tests methods common to all properties
    '''
    global DESC
    
    print "--testBasicROProperty--"
    print "Testing ", propRef, " attributes..."
    
    print "name:", propRef._get_name()
    print "characteristic_component_name:", propRef._get_characteristic_component_name()
    print "description:", propRef._get_description()
    print "format:", propRef._get_format()
    print "units:", propRef._get_units()
    print "resolution:", propRef._get_resolution()
    print "default_timer_trigger:", propRef._get_default_timer_trigger()
    print "min_timer_trigger:", propRef._get_default_timer_trigger()
    print "default_value:", propRef._get_default_value()
    #print ":", propRef._get_()
    
    print
    print "Testing ", propRef, " methods..."
    print "get_characteristic_by_name:", propRef.get_characteristic_by_name("no_name")
    print "find_characteristic:", propRef.find_characteristic("no regular expression")
    print "get_all_characteristics:", propRef.get_all_characteristics()
    print "get_sync:", propRef.get_sync()
    print "get_async:", propRef.get_async(cbRef, DESC)
    #print "get_history:", propRef.get_history(0)
    print "create_monitor:", propRef.create_monitor(cbRef, DESC)
    print "create_postponed_monitor:", propRef.create_postponed_monitor(10000000L, cbRef, DESC)
    #print ":", propRef.
#------------------------------------------------------------------------------
def testComplexROProperty(propRef, cbRef):
    '''
    '''
    global DESC
    
    testBasicROProperty(propRef, cbRef)

    print "--testComplexROProperty--"
    print "Testing ", propRef, " attributes..."
    
    print "min_delta_trigger:", propRef._get_min_delta_trigger()
    print "graph_min:", propRef._get_graph_min()
    print "graph_max:", propRef._get_graph_max()
    print "min_step:", propRef._get_min_step()
    print "alarm_low_on:", propRef._get_alarm_low_on()
    print "alarm_low_off:", propRef._get_alarm_low_off()
    print "alarm_high_on:", propRef._get_alarm_high_on()
    print "alarm_high_off:", propRef._get_alarm_high_off()
    #print ":", propRef._get_()

    print
    print "Testing ", propRef, " methods..."
    #print ":", propRef.new_subscription_Alarm(AlarmDouble()._this(), DESC)
    #print ":", propRef.
#------------------------------------------------------------------------------
def testPatternROProperty(propRef, cbRef):
    '''
    '''
    testBasicROProperty(propRef, cbRef)

    print "--testPatternROProperty--"
    print "Testing ", propRef, " attributes..."

    print "bitDescription:", propRef._get_bitDescription()
    print "whenSet:", propRef._get_whenSet()
    print "whenCleared:", propRef._get_whenCleared()
    #print ":", propRef._get_()

    print
    print "Testing ", propRef, " methods..."
    #print ":", propRef.new_subscription_Alarm(AlarmDouble()._this(), DESC)
    #print ":", propRef.
#------------------------------------------------------------------------------
def testBasicRWProperty(propRef, cbRef, newVal):
    '''
    Tests methods common to all properties
    '''
    global DESC, CBVOID
    
    print "--testBasicRWProperty--"
    print "Testing ", propRef, " attributes..."
    
    print "name:", propRef._get_name()
    print "characteristic_component_name:", propRef._get_characteristic_component_name()
    print "description:", propRef._get_description()
    print "format:", propRef._get_format()
    print "units:", propRef._get_units()
    print "resolution:", propRef._get_resolution()
    print "default_timer_trigger:", propRef._get_default_timer_trigger()
    print "min_timer_trigger:", propRef._get_default_timer_trigger()
    print "default_value:", propRef._get_default_value()
    #print ":", propRef._get_()
    
    print
    print "Testing ", propRef, " methods..."
    print "get_characteristic_by_name:", propRef.get_characteristic_by_name("no_name")
    print "find_characteristic:", propRef.find_characteristic("no regular expression")
    print "get_all_characteristics:", propRef.get_all_characteristics()
    print "get_sync:", propRef.get_sync()
    print "get_async:", propRef.get_async(cbRef, DESC)
    print "set_sync:", propRef.set_sync(newVal)
    print "set_async:", propRef.set_async(newVal, CBVOID, DESC)
    print "set_nonblocking:", propRef.set_nonblocking(newVal)
    #print "get_history:", propRef.get_history(0)
    print "create_monitor:", propRef.create_monitor(cbRef, DESC)
    print "create_postponed_monitor:", propRef.create_postponed_monitor(10000000L, cbRef, DESC)
    #print ":", propRef.
#------------------------------------------------------------------------------
def testComplexRWProperty(propRef, cbRef, newVal):
    '''
    '''
    global CBVOID, DESC
    
    testBasicRWProperty(propRef, cbRef, newVal)

    print "--testComplexRWProperty--"
    print "Testing ", propRef, " attributes..."
    
    print "min_value:", propRef._get_min_value()
    print "max_value:", propRef._get_max_value()

    print
    print "Testing ", propRef, " methods..."
    print "increment:", propRef.increment(CBVOID, DESC)
    print "decrement:", propRef.decrement(CBVOID, DESC)
#------------------------------------------------------------------------------
def testPatternRWProperty(propRef, cbRef, newVal):
    '''
    '''
    testBasicRWProperty(propRef, cbRef, newVal)

    print "--testPatternRWProperty--"
    print "Testing ", propRef, " attributes..."

    print "bitDescription:", propRef._get_bitDescription()
    print "whenSet:", propRef._get_whenSet()
    print "whenCleared:", propRef._get_whenCleared()
    #print ":", propRef._get_()

    print
    print "Testing ", propRef, " methods..."
    #print ":", propRef.
    
#------------------------------------------------------------------------------
# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()
DESC   = ACS.CBDescIn(0L, 0L, 0L)
CBVOID = CBvoid()._this()

pybaci = simpleClient.getComponent("PYBACI1")
print "----------------------------------------------------------------"
testBasicROProperty(pybaci._get_stringROProp(), CBstring()._this())
print "----------------------------------------------------------------"
testBasicROProperty(pybaci._get_strSeqProp(), CBstringSeq()._this())
print "----------------------------------------------------------------"
testPatternROProperty(pybaci._get_patternROProp(),CBpattern()._this())
print "----------------------------------------------------------------"
testComplexROProperty(pybaci._get_doubleROProp(), CBdouble()._this())
print "----------------------------------------------------------------"
testComplexROProperty(pybaci._get_longROProp(), CBlong()._this())
print "----------------------------------------------------------------"
testComplexROProperty(pybaci._get_longLongROProp(), CBlongLong()._this())
print "----------------------------------------------------------------"
testComplexROProperty(pybaci._get_uLongLongROProp(), CBuLongLong()._this())
print "----------------------------------------------------------------"
testComplexROProperty(pybaci._get_doubleSeqROProp(), CBdoubleSeq()._this())
print "----------------------------------------------------------------"
testComplexROProperty(pybaci._get_longSeqROProp(), CBlongSeq()._this())
print "----------------------------------------------------------------"
print "----------------------------------------------------------------"
testBasicRWProperty(pybaci._get_stringRWProp(), CBstring()._this(), "newstring")
print "----------------------------------------------------------------"
testPatternRWProperty(pybaci._get_patternRWProp(),CBpattern()._this(), 3)
print "----------------------------------------------------------------"
testComplexRWProperty(pybaci._get_doubleRWProp(), CBdouble()._this(), 3.14)
print "----------------------------------------------------------------"
testComplexRWProperty(pybaci._get_longRWProp(), CBlong()._this(), 123)
print "----------------------------------------------------------------"
testComplexRWProperty(pybaci._get_longLongRWProp(), CBlongLong()._this(), 1234L)
print "----------------------------------------------------------------"
testComplexRWProperty(pybaci._get_uLongLongRWProp(), CBuLongLong()._this(), 12345L)
print "----------------------------------------------------------------"
testComplexRWProperty(pybaci._get_doubleSeqRWProp(), CBdoubleSeq()._this(), [1.0, 2.0, 3.0])
print "----------------------------------------------------------------"
testComplexRWProperty(pybaci._get_longSeqRWProp(), CBlongSeq()._this(), [10, 20, 30])
print "----------------------------------------------------------------"
print "----------------------------------------------------------------"
testComplexROProperty(pybaci._get_timestampROProp(), CBlongSeq()._this())
print "----------------------------------------------------------------"

#property = pybaci._get_strSeqProp()
#cbMon = CBstringSeq(archive=1)
#cbMonServant = cbMon._this()
## Create the real monitor registered with PYBACI1
#actMon = property.create_monitor(cbMonServant, DESC)
## Tell PYBACI1 that the monitor's working method should be invoked 1 once second
#actMon.set_timer_trigger(10000000)
## destroy the monitor after ten seconds
#from time import sleep
#sleep(10)
#actMon.destroy()
#sleep(5)
#print cbMon.values

simpleClient.releaseComponent("PYBACI1")
simpleClient.disconnect()
print "The end __oOo__"

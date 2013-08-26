# @(#) $Id: Pattern.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
#
# Copyright (C) 2001
# Associated Universities, Inc. Washington DC, USA.
#
# Produced for the ALMA project
#
# This library is free software; you can redistribute it and/or modify it under
# the terms of the GNU Library General Public License as published by the Free
# Software Foundation; either version 2 of the License, or (at your option) any
# later version.
#
# This library is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
# details.
#
# You should have received a copy of the GNU Library General Public License
# along with this library; if not, write to the Free Software Foundation, Inc.,
# 675 Massachusetts Ave, Cambridge, MA 02139, USA.  Correspondence concerning
# ALMA should be addressed as follows:
#
# Internet email: alma-sw-admin@nrao.edu
# "@(#) $Id: Pattern.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2004/07/21  Created.
#------------------------------------------------------------------------------

'''
This module provides an implementation of the Ppattern IDL interface
'''

__version__ = "$Id: Pattern.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from traceback import print_exc
#--CORBA STUBS-----------------------------------------------------------------
import ACS__POA
from ACS import RED, YELLOW, GREEN, GREY  #Condition enumerations
#--ACS Imports-----------------------------------------------------------------
from ACSImpl.GenericProperty     import GenericProperty
from ACSImpl.Monitors            import Monitorpattern
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
#--P property------------------------------------------------------------------
#------------------------------------------------------------------------------
class Ppattern(GenericProperty):
    '''
    Properties can be derived from Ppattern only if their IDL derives from
    ACS::Ppattern.
    '''
    #--------------------------------------------------------------------------
    def __init__(self, name, charCompRef, devIORef):
        '''
        Constructor

        Params:
        - name is the quite literally the name of the property
        - charCompRef is the characteristic component object which contains this
        property
        - devIORef is a reference to a DevIO to be used with this property

        Returns: Nothing

        Raises: Nothing.
        '''
        GenericProperty.__init__(self, name, charCompRef, devIORef)
        
    #--------------------------------------------------------------------------
    def coerceToPropertyType(self, value=None):
        '''
        Overriden.
        '''
        #something went wrong. Return default value
        if value==None:
            return 0L
        
        try:
            #coerce into an int type
            return eval("long(" + value + ")")
        except:
            #warn them about CDB access
            self.getLogger().logAlert("Unble to coerce '" + str(value) + "' into the correct type!")
            print_exc()
            #return an acceptable default value instead...an empty sequence
            return 0L
    #--------------------------------------------------------------------------
    def getMonitorObject(self, scheduler, timeoutID):
        '''
        Helper method returns a monitor of the correct type.
        '''
        return Monitorpattern(scheduler, timeoutID)
    #--------------------------------------------------------------------------
    def _get_bitDescription(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute stringSeq bitDescription;
        '''
        try:
            #should be in the form "abc, xyz,  nypd, etc"
            retVal = str(self.getCDBDict()['bitDescription'])
            #now in the form of "abc,xyz,nypd,etc" (e.g., spaced removed)
            retVal = retVal.replace(" ", "").replace("\t","")
            #now it has been transformed into ["abc", "xyz", "nypd", "etc"]
            retVal = retVal.split(",")
            return retVal
            
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return []
    #--------------------------------------------------------------------------
    def getConditionSeq(self, attrName):
        '''
        Helper method which gets a list of ConditionSeq enumerations from the CDB.

        Parameters: attrName is the name of the ConditionSeq attribute within the
        CDB

        Returns: a ConditionSeq

        Raises: Nothing
        '''
        try:
            #should be in the form "RED,   YELLOW"
            retVal = str(self.getCDBDict()[attrName])
            #now in the form of "RED,YELLOW" (e.g., spaced removed)
            retVal = retVal.replace(" ", "").replace("\t","")
            #now it has been transformed into ["RED", "YELLOW"]
            retVal = retVal.split(",")

            #convert the strings to real enumerations!
            for i in range(0, len(retVal)):
                #sanity check first
                if (retVal[i]!="0")and(retVal[i]!="1")and(retVal[i]!="2")and(retVal[i]!="3"): 
                    if (retVal[i].upper()!="RED")and(retVal[i].upper()!="YELLOW")and(retVal[i].upper()!="GREEN")and(retVal[i].upper()!="GREY"):
                        self.getLogger().logCritical("The '" + str(retVal[i]) +
                                                     "' value retrieved from the CDB is invalid. Returning an empty ConditionSeq!")
                        return []
                elif (retVal[i]=="0"):
                    retVal[i] = "RED"
                elif (retVal[i]=="1"):
                    retVal[i] = "YELLOW"
                elif (retVal[i]=="2"):
                    retVal[i] = "GREEN"
                else:
                    retVal[i] = "GREY"
                        
                retVal[i] = eval(retVal[i])
                            
            return retVal
            
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return []
    #--------------------------------------------------------------------------
    def _get_whenSet(self):
        '''
        Implementation of IDL attribute.

        readonly attribute ConditionSeq whenSet;
        '''
        return self.getConditionSeq("whenSet")
    #--------------------------------------------------------------------------
    def _get_whenCleared(self):
        '''
        Implementation of IDL attribute.

        readonly attribute ConditionSeq whenCleared;
        '''
        return self.getConditionSeq("whenCleared")
#------------------------------------------------------------------------------
#--RO property-----------------------------------------------------------------
#------------------------------------------------------------------------------
class ROpattern(ACS__POA.ROpattern, Ppattern):
    '''
    Properties can be derived from ROpattern only if their IDL derives from
    ACS::ROpattern.
    '''
    #--------------------------------------------------------------------------
    def __init__(self, name, charCompRef, devIORef=None):
        '''
        Constructor

        Params:
        - name is the quite literally the name of the property
        - charCompRef is the characteristic component object which contains this
        property
        - devIORef is a reference to a DevIO to be used with this property

        Returns: Nothing

        Raises: Nothing.
        '''
        Ppattern.__init__(self, name, charCompRef, devIORef)
        return
#-----------------------------------------------------------------------------
#--RW property----------------------------------------------------------------
#-----------------------------------------------------------------------------
class RWpattern(ACS__POA.RWpattern, ROpattern):
    '''
    Properties can be derived from ROpattern only if their IDL derives from
    ACS::ROpattern.
    '''
    #-------------------------------------------------------------------------
    def __init__(self, name, charCompRef, devIORef=None):
        '''
        Constructor

        Params:
        - name is the quite literally the name of the property
        - charCompRef is the characteristic component object which contains this
        property
        - devIORef is a reference to a DevIO to be used with this property

        Returns: Nothing

        Raises: Nothing.
        '''
        ROpattern.__init__(self, name, charCompRef, devIORef)
        return
#---------------------------------------------------------------------------

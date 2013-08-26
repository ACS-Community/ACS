# @(#) $Id: Enum.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
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
# "@(#) $Id: Enum.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2004/07/21  Created.
#------------------------------------------------------------------------------

'''
This module provides an implementation of the Penum IDL interface:
'''

__version__ = "$Id: Enum.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from traceback import print_exc
#--CORBA STUBS-----------------------------------------------------------------
import ACS__POA
from ACS import RED, YELLOW, GREEN, GREY  #Condition enumerations
from ACS import CBDescOut
import CORBA
#--ACS Imports-----------------------------------------------------------------
from ACSImpl.GenericProperty     import GenericProperty
from ACSImpl.Monitors            import Monitorpattern
from Acspy.Util.ACSCorba         import interfaceRepository
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
#--P property------------------------------------------------------------------
#------------------------------------------------------------------------------
class Penum(GenericProperty):
    '''
    Properties can be derived from Penum only if their IDL derives from
    ACS::Penum.
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
        #------------------------------------------------------------------------------
        #determine the real type of the BACI property using the IFR
        #First determine the property's name
        self.enumIFRName = None
        
        IR = interfaceRepository()
        interf = IR.lookup_id(self._NP_RepositoryId)
        
        #determine the underlying enumeration type
        for attr in interf._narrow(CORBA.InterfaceDef).describe_interface().attributes:
            #we can use the default_value to get at an instance of the real enumeration
            if attr.name == "default_value":
                #ifr name of the enum
                self.enumIFRName = attr.type.id()
                break

        #determine the IDL type
        self.enumName = self.enumIFRName.split(':')[1].split('/')[1:]
        self.modName = self.enumName[0]

        #convert the list to a stringified Python package structure which can
        #be used with eval
        self.enumName = reduce((lambda x, y : str(x) + '.' + str(y)), self.enumName)

        #now comes the complicated part...importing the correct CORBA stub
        #without polluting the local namespace...
        self.tGlobals = {}
        self.tLocals  = {}
        exec "import " + self.modName in self.tGlobals, self.tLocals
        
        GenericProperty.__init__(self, name, charCompRef, devIORef)
        
        return
    #--------------------------------------------------------------------------
    def coerceToPropertyType(self, value=None):
        '''
        Overriden.
        '''
        #something went wrong. Return default value
        if value==None:
            #just use the first enum!
            return eval(self.enumName + "._items[0]", self.tGlobals, self.tLocals)
        
        try:
            #coerce into an enum type
            return eval(self.enumName + "._items[" + value + "]",
                        self.tGlobals,
                        self.tLocals) 
        except:
            #warn them about CDB access
            self.getLogger().logAlert("Unble to coerce '" + str(value) + "' into the correct type!")
            print_exc()
            #return an acceptable default value instead...an empty sequence
            return eval(self.enumName + "._items[0]", self.tGlobals, self.tLocals)
    #--------------------------------------------------------------------------
    def getMonitorObject(self, scheduler, timeoutID):
        '''
        Helper method.used to return a monitor of the correct type.
        '''
        return Monitorpattern(scheduler, timeoutID)
    #--------------------------------------------------------------------------
    def monitorRunner(self, cb, desc):
        '''
        Helper method
        '''
        allList = self._get_allStates()
        (value, compl) = self.get_sync()
        cb.working(long(allList.index(value)), compl, CBDescOut(0L, desc.id_tag))     
        return
    #--------------------------------------------------------------------------
    def _get_statesDescription(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute stringSeq bitDescription;
        '''
        try:
            #should be in the form "abc, xyz,  nypd, etc"
            retVal = str(self.getCDBDict()['statesDescription'])
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
    def _get_condition(self):
        '''
        Implementation of IDL attribute.

        readonly attribute ACS::ConditionSeq condition;
        '''
        return self.getConditionSeq("condition")
    #--------------------------------------------------------------------------
    def _get_allStates(self):
        '''
        Implementation of IDL attribute.

        readonly attribute ConditionSeq whenCleared;
        '''
        return eval("list(" + self.enumName + "._items)", self.tGlobals, self.tLocals)
    #--------------------------------------------------------------------------
    def _get_alarm_on(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) alarm_on;
        '''
        try:
            return [self.coerceToPropertyType(str(self.getCDBDict()['alarm_on']))]
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return [self.coerceToPropertyType(None)]
    #--------------------------------------------------------------------------
    def _get_alarm_off(self):
        '''
        Implementation of the IDL attribute.
        
        readonly attribute (unknown type) alarm_off;
        '''
        try:
            return [self.coerceToPropertyType(str(self.getCDBDict()['alarm_off']))]
        except:
            #warn them about CDB access
            self.getLogger().logInfo("Some problem occurred when attempting to retrieve data from the ACS CDB")
            print_exc()
            #return an acceptable default value instead.
            return [self.coerceToPropertyType(None)]
    #--------------------------------------------------------------------------
    def get_async(self, cb, desc):
        '''
        Implementation of the IDL method.
        
        void get_async (in CBstringSeq cb, in CBDescIn desc);
        '''
        self.getLogger().logInfo('Not really asynchronous for now...')
        retVal, compl = self.get_sync()
        allList = self._get_allStates()
        
        cb.done(long(allList.index(retVal)),
                compl,
                CBDescOut(0L, desc.id_tag))
        return 
#------------------------------------------------------------------------------
_enumClassDict = {}

def getEnumClass(repositoryID):
    '''
    Function designed to return the CORBA stub enumeration class given the CORBA
    enums interface repository ID.

    Parameters: repositoryID is the interface repository ID of the enum

    Returns: a Python class

    Raises: ???
    '''
    global _enumClassDict

    if _enumClassDict.has_key(repositoryID):
        return _enumClassDict[repositoryID]
    
    #first convert the repos ID to the name of a Python class
    enumName = repositoryID.split(':')[1].split('/')[1:]
    #should be in the form of [ 'somemodName__POA', 'anotherModName',
    # ..., 'ROenumname' ] now 
    enumName[0] = enumName[0] + "__POA"
    #import the base module
    mod = __import__(enumName[0])

    #keep getting modules until we get to last one containing the enum
    #property interface declaration
    for i in range(1, len(enumName)-1):
        mod = getattr(mod, enumName[i])

    #at this point we should be at the final IDL module and we can
    #directly access 
    klass = getattr(mod, enumName[len(enumName)-1])

    class tClass(klass, Penum):
        '''
        Temporary class.
        '''
        pass

    #save this for the next time this class is needed
    _enumClassDict[repositoryID] = tClass
    return tClass
#------------------------------------------------------------------------------


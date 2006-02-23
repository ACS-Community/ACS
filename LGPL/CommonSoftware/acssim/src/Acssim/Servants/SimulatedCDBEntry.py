# @(#) $Id: SimulatedCDBEntry.py,v 1.2 2005/11/23 18:40:03 dfugate Exp $
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
# "@(#) $Id: SimulatedCDBEntry.py,v 1.2 2005/11/23 18:40:03 dfugate Exp $"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/12/09  Created.
#------------------------------------------------------------------------------
'''
TODO:

'''
#--REGULAR IMPORTS-------------------------------------------------------------
from operator import isSequenceType
#--CORBA STUBS-----------------------------------------------------------------

#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.CDBAccess import CDBaccess
from Acspy.Util.XmlObjectifier import XmlObject

from Acssim.Servants.SimulatedEntry import SimulatedEntry
#--GLOBALS---------------------------------------------------------------------

#------------------------------------------------------------------------------
class SimulatedCDBEntry(SimulatedEntry):
    '''
    Class derived from SimulatedEntry to be used only with the CDB. In other words,
    this class searchs the CDB for entries describing method/attribute return
    values.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, compname):
        '''
        Constructor.

        Paramters:
        - compname is the name of the component being simulated

        Returns: Nothing

        Raises: ???
        '''
        #superclass constructor
        SimulatedEntry.__init__(self, compname)
        
        #first get access to the CDB
        self.cdb = CDBaccess()     

        #bool value showing whether the CDB entry exists or not
        self.exists=0
        
        try:
            #make sure the entry exists first of all...
            t_xml = self.cdb.getField("alma/simulated/" + self.compname)
            self.exists=1
        except:
            #since using CDB entries for simulated components is optional,
            #there is no point in propagating this exception. just return
            #control instead
            return

        #create an xml helper object
        xml_obj = XmlObject(xmlString = t_xml)

        #get the corba methods
        self.getCorbaMethods(xml_obj)

        #get the corba attributes
        self.getCorbaAttributes(xml_obj)

    #--------------------------------------------------------------------------
    def getCorbaMethods(self, xml_obj):
        '''
        '''
        #methods is the somewhat formatted data taken from the XML. not really
        #nice enough to work with yet.
        try:
            methods = xml_obj.SimulatedComponent._corbaMethod
            if isSequenceType(methods)==0:
                methods = [ methods ]
        except:
            return

        #for each method in the list
        for dom in methods:
            #dictionary defining the method
            tDict = {}

            #extract the method name
            methname = dom.getAttribute('Name')

            #set the timeout
            tDict['Timeout'] = float(dom.getAttribute('Timeout'))

            #get the code to be executed yielding a return value
            tDict['Value'] = dom.getValue().rstrip().lstrip().split('\n')
        
            #save the dictionary
            self.setMethod(methname, tDict)
    #--------------------------------------------------------------------------
    def getCorbaAttributes(self, xml_obj):
        '''
        '''
        #attributes is the somewhat formatted data taken from the XML. not really
        #nice enough to work with yet.
        try:
            attributes = xml_obj.SimulatedComponent._corbaAttribute
            if isSequenceType(attributes)==0:
                attributes = [ attributes ]
        except:
            return

        #for each method in the list
        for dom in attributes:
            #dictionary defining the attribute
            tDict = {}

            #extract the attribute name
            attrname = dom.getAttribute('Name')

            #set the timeout
            tDict['Timeout'] = float(dom.getAttribute('Timeout'))

            #get the code to be executed yielding a return value
            tDict['Value'] = dom.getValue().rstrip().lstrip().split('\n')
        
            #save the dictionary
            self.setMethod(attrname, tDict)
    #--------------------------------------------------------------------------

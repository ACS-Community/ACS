# @(#) $Id: CDBAccess.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
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
#------------------------------------------------------------------------------
'''
This module defines classes capable of reading data from the ACS configuration
database.

TODO:
- Exception handling!!!
- Fix bug involving an XML with a sequence of identical elements
'''

__revision__ = "$Id: CDBAccess.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

#--REGULAR IMPORTS-------------------------------------------------------------
from   xml.sax             import ContentHandler, parseString
from   Acspy.Util.ACSCorba import cdb
#--CORBA STUBS-----------------------------------------------------------------

#--GLOBALS---------------------------------------------------------------------
_DEBUG = 0
#------------------------------------------------------------------------------
class CDBhandler(ContentHandler):
    '''
    CDBhandler is a simple class designed to extract the attributes of an XML
    element. Developers should not use this class directly...
    '''
    #--------------------------------------------------------------------------
    def __init__ (self, name):
        '''
        The constructor simply saves the name of the XML element we are after.

        Parameters: name is the XML elements name

        Returns: Nothing

        Raises: Nothing
        '''
        #Call superclass constructor
        ContentHandler.__init__(self)

        #set some member variables
        self.element_name  = name
        self.tempName = ""
        self.return_values = []

        if _DEBUG: # pragma: NO COVER
            print self.__dict__
    #--------------------------------------------------------------------------
    def startElement(self, name, attrs):
        '''
        Copies all attributes to a member variable if it is the correct element.
        
        Parameters:
        - name is the XML elements name
        - attrs is a dictionary of the names attributes

        Returns: Nothing

        Raises: ???
        '''
        if _DEBUG: # pragma: NO COVER
            print "SE", self.tempName, name
            
        self.tempName = self.tempName + name.encode('ascii')

        if _DEBUG: # pragma: NO COVER
            print "   SE Equality: '%s' '%s'" % (self.tempName, self.element_name)
            print "   SE Equality:", type(self.tempName), type(self.element_name)
            print "   SE Equality:", len(self.tempName), len(self.element_name)
            
        if self.tempName == self.element_name:  #we're done
            if _DEBUG: # pragma: NO COVER
                print "SE Match:", self.tempName
            return_value = {}
            for name in attrs.getNames():
                return_value[str(name)] = str(attrs.getValue(name))

            self.return_values.append(return_value)

        self.tempName = self.tempName + "/"
    #--------------------------------------------------------------------------
    def endElement(self, name):
        '''
        Removes the current element name from tempName after we are done using
        it.
        
        Parameters: name is the XML elements name

        Returns: Nothing

        Raises: ???
        '''
        if _DEBUG: # pragma: NO COVER
            print "EE", name, self.tempName, self.tempName.replace(name.encode('ascii') + "/", "", 1)
        self.tempName = self.tempName.replace(name.encode('ascii') + "/", "", 1)
#------------------------------------------------------------------------------
class CDBaccess(object):
    '''
    CDBaccess provides a readonly interface into the ACS configuration database.
    It does NOT use DAOs, but instead uses the XML strings returned by the DAL.
    It has an internal cache so that multiple requests on the same XML file do
    not result in excess network traffic.
    '''
    #--------------------------------------------------------------------------
    def __init__ (self):
        '''
        The constructor just initializes a few member variables and gets direct
        access to the DAL.

        Parameters: None

        Returns: Nothing

        Raises: ???
        '''
        self.cache = {}

        #Get the DAL
        self.m_dal = cdb()
    #--------------------------------------------------------------------------   
    def getField(self, name, caching=1):
        '''
        Returns an entire XML file in the form of a string. Performs caching
        to improve network performance.
        
        Parameters:
        - name is the name of the XML file within the CDB ("alma:MOUNT1")
        - if caching is 1 and the XML file has been retrieved before, there
        will be no network call out to the CDB from this invocation. Used to
        increase performance.

        Returns: XML record if exists and None otherwise

        Raises: ???
        '''
        if self.cache.has_key(str(name)) and caching:
            return self.cache[str(name)]
        else:
            self.cache[str(name)] = str(self.m_dal.get_DAO(name))
            return self.cache[str(name)]
    #--------------------------------------------------------------------------
    def getElement(self, xml_name, element_name, caching=1):
        '''
        This method returns all of the attributes in the form of a dictionary
        for a given XML element within an XML file.

        Parameters:
        - xml_name is the name of the XML file (e.g., "alma/MOUNT1")
        - element_name is the name of the element (e.g, "MOUNT/actAz",
          where acsAz is an XML element contianing all attributes
          representing the characteristics of a BACI property).
        
        Returns: a dictionary full of attributes or None

        Raises: ???
        '''
        handler = CDBhandler(element_name)
        parseString(self.getField(xml_name, caching), handler)
        return handler.return_values
#------------------------------------------------------------------------------


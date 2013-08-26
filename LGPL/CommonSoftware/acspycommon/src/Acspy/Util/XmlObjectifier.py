# Copyright (C) 2004-2007
# Max-Planck-Institut fuer Radioastronomie Bonn
#
# Produced for the ALMA and APEX projects
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
#
# Correspondence concerning APEX should be addressed as follows:
#
# Internet email: dmuders@mpifr-bonn.mpg.de

# The Python XML Objectifier
#
# Who		   When	       What
#
# D.Muders, MPIfR  2007-08-20  Renamed internal element name to _0elementName to
#                              allow for the string "name" to be an XML element
#                              name.
# D.Muders, MPIfR  2005-08-16  Added "setValue" method to "XmlElement" class.
# D.Muders, MPIfR  2004-12-14  - Improved name space mapping.
#			       - Added switches to skip arbitrary leading
#				 characters in name space definitions.
#			       - Made name space mapping optional.
#			       - Added lots of documentation.
# D.Muders, MPIfR  2004-12-10  - Renamed XmlFile class to XmlObject
#			       - Allow to parse XML strings
# D.Muders, MPIfR  2004-12-09  - Renamed elementName to elementName_obj
#			       - Renamed elementName_list to elementName
#			       - Interpret name spaces and prepend to
#				 elementName
# C.Koenig, MPIfR  2004-02-09  - Added method getValue() to class XmlElement
#                              - Added the XmlObjectifierError class
# C.Koenig, MPIfR  2004-01-01  Creation of module


"""
This module is used to create native Python objects representing an XML document
rendering the elements into a hierarchical tree. Name spaces can optionally be
mapped into the element names by specifying 'mapNameSpaces = 1'. Leading
characters can be omitted in the name space mapping using the 'skipChars'
argument.

Characters that are not allowed in Python names ('.', '-', '/', ':') are mapped
to '_'.

The resulting Python object can be modified and serialized into XML again using
the 'writexml' method.

Example usage:

import XmlObjectifier
xmlObject = XmlObjectifier.XmlObject(xmlString = <XML string>,
                                     skipChars = <string>)

or

xmlObject = XmlObjectifier.XmlObject(fileName = '<file name>',
                                     skipChars = <string>)

This example XML document:

<?xml version="1.0" encoding="ISO-8859-1"?>
<!-- edited with XMLSPY v5 U (http://www.xmlspy.com) by D. Muders (MPIfR) -->
<TelCalResult xmlns="Alma/TelCalResult"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="Alma/TelCalResult/TelCalResult-single.xsd">
        <TelCalResultEntity entityId="2718281828" entityIdEncrypted=""
            entityVersion="0.1"/>
        <TelCalResultRef entityId="2718281827"/>
        <SchedBlockRef entityId="31415926534"/>
        <TelCalResultDetail>
                <ResultKind>FocusOffset</ResultKind>
                <ScanID>1789</ScanID>
                <NumAntennas>2</NumAntennas>
                <AntennaID>64</AntennaID>
                <AntennaID>42</AntennaID>
                <FocusOffset>-0.25</FocusOffset>
                <FocusOffset>-0.34</FocusOffset>
        </TelCalResultDetail>
</TelCalResult>

can then be navigated in the Python object like this:

#!/usr/bin/env python

import XmlObjectifier

def printInfo(focusResult):
    scanID = focusResult.TelCalResult.TelCalResultDetail.ScanID.getValue()
    kind = focusResult.TelCalResult.TelCalResultDetail.ResultKind.getValue()
    numAnts = focusResult.TelCalResult.TelCalResultDetail.NumAntennas.getValue()

    print 'This is a %s result entity. Scan ID: %d. Number of antennas: %d' % \
        (kind, scanID, numAnts)
    print

    for ant in xrange(numAnts):
        antID = \
            focusResult.TelCalResult.TelCalResultDetail.AntennaID[ant].getValue()
        focusOffset = \
            focusResult.TelCalResult.TelCalResultDetail.FocusOffset[ant].getValue()
        print 'Antenna #%d focus offset: %.1f' % (antID, focusOffset)

# Objectify XML
focusResult = XmlObjectifier.XmlObject(fileName = 'FocusResult.xml')

# Print object summary
print 'Original focus result:\n'
printInfo(focusResult)

# Optionally modify elements
focusResult.TelCalResult.TelCalResultDetail.ScanID.setValue(1790)
focusResult.TelCalResult.TelCalResultDetail.AntennaID[0].setValue(24)
focusResult.TelCalResult.TelCalResultDetail.FocusOffset[0].setValue(0.3)
focusResult.TelCalResult.TelCalResultDetail.AntennaID[1].setValue(25)
focusResult.TelCalResult.TelCalResultDetail.FocusOffset[1].setValue(0.5)

# Print object summary
print '\n\nNew focus result:\n'
printInfo(focusResult)

# Write XML to a new file
f = open('FocusResultNew.xml', 'w+')
focusResult.writexml(f, '')
f.close()
"""


import xml.dom.minidom as minidom
from traceback import print_exc

class _XmlObject:
    '''
    This class definition is used for the additional "elementName_obj"
    objects in the hierarchy that allow to access child nodes via the __call__
    method.
    '''

    def __init__(self, elementsList):
        self.elementsList = elementsList
        
    def __call__(self, number=None, text=None, **keywords):
        if keywords == {} and text == None:
            if len(self.elementsList) > 1:
                if number == None:
                    msg = 'More than one XmlElement of type ' + \
                        str(self.elementsList[0]._0elementName)+'. '
                    msg = msg + 'Select one py passing a number (0 - ' + \
                        str(len(self.elementsList)-1) + ')'
                    raise XmlObjectifierError(msg)
                elif number in range(0,len(self.elementsList)):
                    result = self.elementsList[number]
                else:
                    msg = 'KeyNumber out of range'
                    raise XmlObjectifierError(msg)
                return result
            else:
                return self.elementsList[0]
        else:
            matches = 0
            matchList = []
            for element in self.elementsList:
                if text != None:
                    for item in element.childNodes:
                        if item.nodeType == 3:
                            if str(item.data) == str(text):
                                matches = 1
                            else:
                                matches = 0
                for key in keywords:
                    if element.hasAttribute(key):
                        if element.getAttribute(key) == keywords[key]:
                            matches = 1
                        else:
                            matches = 0
                            break
                    else:
                        matches = 0
                        break
                if matches == 1:
                    matchList.append(element)
            if len(matchList) > 1:
                raise KeyError, 'More than one result found'
            elif len(matchList) < 1:
                return None
            else:
                return matchList[0]


def _createLists(xmlObject, mapNameSpaces, nameSpaceMapping, skipChars):

    '''
    Generate lists of elements if one kind of element exists several times on the
    same level. Otherwise map it into a scalar.
    '''

    if xmlObject.hasChildNodes():
        items = []
        for element in xmlObject.childNodes:
            if element.nodeType == 1:
                elementName = str(element.nodeName)
		# '-' is not allowed in Python names
                elementName = elementName.replace('-','_')

		# Handle name spaces
                sptr = elementName.find(':')
                if (sptr != -1):
                    nameSpace = elementName[:sptr]
                    nameSpaceKey = nameSpace + ':'
		    # Keep new name spaces for later use
                    if (not (nameSpaceMapping.has_key(nameSpaceKey))):
                        if (mapNameSpaces):
                            path = element.getAttribute('xmlns:' + nameSpace)
                            # Skip leading strings if desired
                            if (skipChars):
                                path = path.replace(skipChars, '')
			    # Skip initial URL characters if any
			    if path.startswith('http://'):
                                path = path[len('http://'):]
			    # Python names must not contain the '.' character
                            path = path.replace('.','_')
			    # Python names must not contain the '/' character
                            path = path.replace('/','_') + '_'
                            nameSpaceMapping[nameSpaceKey] = path
			else:
                            nameSpaceMapping[nameSpaceKey] = ''

                    elementName = elementName.replace(nameSpaceKey, \
                        nameSpaceMapping[nameSpaceKey])

                if not hasattr(xmlObject,elementName):
                    list = []
                    setattr(xmlObject,elementName,list)
                    items.append(elementName)
                    setattr(xmlObject,elementName+'_obj', \
                        _XmlObject(getattr(xmlObject,elementName)))
                else:
                    list = getattr(xmlObject,elementName)
                myXmlElement = XmlElement(element, mapNameSpaces, nameSpaceMapping, skipChars)
                list.append(myXmlElement)

        # Convert 1-item element lists to scalar elements
        for item in items:
            if (eval('len(xmlObject.%s)' % (item)) == 1):
                exec('tmpItem = xmlObject.%s[0]' % (item))
                delattr(xmlObject, item)
                setattr(xmlObject, item, tmpItem)


def castType(value):

    try:
        value = int(value)
    except ValueError:
        try:
            value = float(value)
        except ValueError:
            value = str(value)
            if value.lower() == 'false':
                value = 0
            elif value.lower() == 'true':
                value = 1
    return value


class XmlObject(minidom.Document):

    '''
    Creates an object representing the XML document wich is to be objectified.
    The XML string passed to the constructor is preferred over any specified 
    XML file.

    Optionally the name space mapping can be turned on by passing mapNameSpaces = 1.

    Leading characters in the name space definitions can be skipped in the mapping
    by passing the optional "skipChars" argument.
    '''

    def __init__(self, xmlString = None, fileName = None, skipChars = '',
        mapNameSpaces = 0):

        # The name space mapping needs to be known on all levels of the object
	# hierarchy.
        nameSpaceMapping = {}

        minidom.Document.__init__(self)
	if (xmlString):
            dom = minidom.parseString(xmlString)
	elif (fileName):
            dom = minidom.parse(fileName)
	else:
	    raise XmlObjectifierError('No XML string or filename specified')
        dom.documentElement.normalize()
        for attr in dir(dom):
            if attr != '__init__':
                setattr(self,attr,getattr(dom,attr))
        _createLists(self, mapNameSpaces, nameSpaceMapping, skipChars)



class XmlElement(minidom.Element):

    '''Creates an object representing an XML tag/element with all of its content.'''


    def __init__(self, element, mapNameSpaces, nameSpaceMapping, skipChars):

        minidom.Element.__init__(self,str(element.nodeName))
        for attr in dir(element):
            if attr != '__init__' and attr != 'getAttribute':
                setattr(self,attr,getattr(element,attr))
        _createLists(self, mapNameSpaces, nameSpaceMapping, skipChars)
        self._0elementName = str(self.nodeName)


    def getAttribute(self,name):

        '''Overwrites the inherited method and returns a value of the right type.'''
        
        result = minidom.Element.getAttribute(self,name)
        result = str(result)
        result = castType(result)
        return result

    
    def getValue(self):

        '''Returns the included TEXT, if present.'''

        if len(self.childNodes) > 1:
            msg = "Xml Element does not seem to be an end point"
            raise XmlObjectifierError(msg)
        elif len(self.childNodes) < 1:
            msg = "Xml Element does not have any child nodes"
            raise XmlObjectifierError(msg)
        
        node = self.childNodes[0]
        if node.nodeType == 3:
            value = castType(node.nodeValue)
            return value
        else:
            msg = "Xml Element does not have any text included"
            raise XmlObjectifierError(msg)

    def setValue(self, value):

        '''Sets the included TEXT.'''

        if len(self.childNodes) > 1:
            msg = "Xml Element does not seem to be an end point"
            raise XmlObjectifierError(msg)
        elif len(self.childNodes) < 1:
            msg = "Xml Element does not have any child nodes"
            raise XmlObjectifierError(msg)
        
        node = self.childNodes[0]
        if node.nodeType == 3:
            node.nodeValue = unicode(value)
        else:
            msg = "Xml Element does not have any text included"
            raise XmlObjectifierError(msg)


class XmlObjectifierError(Exception):
    
    def __init__(self, msg, code=None):
        self.code = code
        self.msg = msg
        
    def __str__(self):
        return `self.msg`


# APEX specific main to load the MBFITS XML definition.
if __name__ == '__main__':
    import interactive
    myXmlObject = XmlObject(fileName = '../../idl/MBFits.xml')
    scanStructure = myXmlObject.Scan()


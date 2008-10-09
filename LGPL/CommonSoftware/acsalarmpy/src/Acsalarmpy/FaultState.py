#! /usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) National Research Council of Canada, 2008 
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
# "@(#) $Id: FaultState.py,v 1.2 2008/10/09 19:13:20 agrimstrup Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# agrimstrup  2008-09-30  created
#

class Properties(dict):
    """
    Collection of name/value pairs that represent properties.
    """
    def toXML(self, amountToIndent=6):
        """
        Create an XML fragment representing the names and values contained
        in this object.

        Parameters: amountToIndent is the level of indentation for this fragment

        Returns:  an indented XML string
        """
        if len(self) > 0:
            pad = '\t'.expandtabs(amountToIndent+3)
            endpad = '\t'.expandtabs(amountToIndent)
            taglist = ['<user-properties>\n']
            for k in self:
                taglist.append('<property name="%s" value="%s"/>\n' % (k, self[k]))
            rtn = pad.join(taglist)
            return endpad.join([rtn, '</user-properties>\n'])
        else:
            return ''

# FaultState constants
ACTIVE_STRING = "ACTIVE"
TERMINATE_STRING = "TERMINATE"
CHANGE_STRING = "CHANGE"
INSTANT_STRING = "INSTANT_STRING";
ASI_PREFIX_PROPERTY_STRING = "ASI_PREFIX_PROPERTY";
ASI_SUFFIX_PROPERTY_STRING = "ASI_SUFFIX_PROPERTY";


class FaultState(object):
    """
    Class representing a single fault to be sent from an alarm source to the LASER
    alarm server.
    """
    def __init__(self, family=None, member=None, code=None):
        """
        Create a fault state object.

        Parameters: family is the name of the alarm family
                    member is the name of the alarm family member
                    code is the error code being reported

        Returns: initialized FaultState object
        """
        if family is not None and member is not None and code is not None:
            self.family = family
            self.member = member
            self.code = code
        else:
            self.family = None
            self.member = None
            self.code = None
        self.userProperties = Properties()
        self.userTimestamp = None
        self.descriptor = None
        self.activatedByBackup = None
        self.terminatedByBackup = None
        

    def toXML(self, amountToIndent=3):
        """
        Create an XML fragment representing the fault's state.

        Parameter:  amountToIndent is the indentation level for this fragment

        Returns: an indented XML string

        Raises:  exception if the family, member or code has not been set correctly
        """
        taglist = []
        pad = '\t'.expandtabs(amountToIndent)
        if self.family is None or self.member is None or self.code is None:
            raise TypeError, "Family, member and code information must be provided"
        taglist.append('<fault-state family="%s" member="%s" code="%d">\n' % (self.family, self.member, self.code))
        if self.descriptor is not None:
            taglist.append('<descriptor>%s</descriptor>\n' % self.descriptor)
        if len(self.userProperties) > 0:
            taglist.append(self.userProperties.toXML(amountToIndent))
        if self.userTimestamp is not None:
            taglist.append(self.userTimestamp.toXML(amountToIndent=0))
        rtn = pad.join(taglist)
        return rtn + '</fault-state>\n'

#
# ___oOo___

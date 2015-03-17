# @(#) $Id: NamingService.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $
#
#    ALMA - Atacama Large Millimiter Array
#    (c) Associated Universities, Inc. Washington DC, USA,  2001
#    (c) European Southern Observatory, 2002
#    Copyright by ESO (in the framework of the ALMA collaboration)
#    and Cosylab 2002, All rights reserved
#
#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU Lesser General Public
#    License as published by the Free Software Foundation; either
#    version 2.1 of the License, or (at your option) any later version.
#
#    This library is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    Lesser General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public
#    License along with this library; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

__revision__ = "$Id: NamingService.py,v 1.1.1.1 2012/03/07 17:40:45 acaproni Exp $"

'''
TODO:
- update documentation to be consistent with the rest of Acspy package
'''

from omniORB              import CORBA
from Acspy.Util.ACSCorba  import nameService

import CosNaming

from traceback import print_exc
#--------------------------------------------------------------------------
class NamingServiceUtil(object):
    """
    This class merely provides simplified access to the Naming Service. It
    provides the following functions:

    __init__    - attach to the naming service
    addObject   - put an object in the naming service
    getObject   - retrieve an object from the naming service

    In practice the NamingService is much more capable (e.g. hierarchical), but
    less convenient to use.
    """
    #--------------------------------------------------------------------------
    def __init__(self, nameServiceArgs):
        """
        Attach to the Naming Service at the supplied host and port number. An
        exception is thrown if this cannot be accomplished (probably the naming
        service is not running).
        """
        self.__nsroot = nameService()
        return
    #--------------------------------------------------------------------------
    def addObject(self, name, object):
        """
        Add object to the Naming Service with name=name and kind="". Note that
        hierarchies are not allowed for, i.e. the objects are placed in the
        first level. object must be a CORBA object, not an arbitrary Python
        object. An exception is thrown on error.
        """
        # TODO - check types
        context = self.__nsroot
        # Following the bible, do not use kind.
        name = [CosNaming.NameComponent(name, "")]
        try:
            context.bind(name, object)
        except CosNaming.NamingContext.AlreadyBound:
            # If name already exists, silently rebind it
            try:
                context.rebind(name, object)
            except CORBA.Exception, e:
                print_exc()
                raise "addObject - could not rebind object. Use ._this?"

    #--------------------------------------------------------------------------
    def getObject(self, name):
        """
        Return an object named "name" from the Naming Service. Kind must be
        equal to "". None is returned if the object cannot be obtained.
        In general you will have to narrow the object reference after you 
        obtain it. Only the first level is searched, not hierarchies.
        TODO - Search for any kind?
        """
        context = self.__nsroot
        name = [CosNaming.NameComponent(name, "")]
        try:
            object = context.resolve(name)
        except Exception, e:
            object = None # Probably: does not exist in the naming service
        return object
    #--------------------------------------------------------------------------
    def delObject(self, name, kind):
        """
        Unbind something from the Naming Service
        """
        context = self.__nsroot
        name = [CosNaming.NameComponent(name, kind)]
        try:
            object = context.unbind(name)
        except Exception, e:
            object = None # Probably: does not exist in the naming service
            print e
        return object


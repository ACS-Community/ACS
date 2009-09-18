# @(#) $Id$
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
# "@(#) $Id: ACSPorts.py,v 1.7 2007/10/11 17:27:06 nbarriga Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# hyatagai  2009/09/18  Created.
#------------------------------------------------------------------------------
'''
This module is used to know the path of the directory contains ACS instances.

TODO:
- nada
'''
#------------------------------------------------------------------------------
__revision__ = "$Id: ACSPorts.py,v 1.7 2007/10/11 17:27:06 nbarriga Exp $"
#--REGULAR IMPORTS-------------------------------------------------------------
from os      import environ
import socket
#----------------------------------------------------------------------------
def getAcsTmpDirectoryPath():
    '''
    Returns the path of the directory contains ACS instances.
  
    Parameters: None
  
    Return: the path of the directory contains ACS instances.
    
    Raises: Nothing
    '''

    if environ.has_key('ACS_TMP'):
        ACS_TMP_DIR = str(environ['ACS_TMP'])
    else:
        hostname = socket.gethostname().split('.')[0]
        ACS_TMP_DIR = str(environ['ACSDATA']) + '/tmp/' + hostname

    return ACS_TMP_DIR



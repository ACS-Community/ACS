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
# "@(#) $Id$"
#
# who       when        what
# --------  ----------  -------------------------------------------------------
# dfugate   2003/12/09  Created.
#------------------------------------------------------------------------------
'''
Module contains helper methods which generate and manipulate known ACS types.
Generally these are defined in baci.idl.
'''
#--REGULAR IMPORTS-------------------------------------------------------------

#--CORBA STUBS-----------------------------------------------------------------
import CORBA
import ACSErr
import ACS
#--ACS Imports-----------------------------------------------------------------
from Acspy.Common.Log       import getLogger
from Acspy.Common.TimeHelper import getTimeStamp
#--GLOBALS---------------------------------------------------------------------
__revision__ = "@(#) $Id$"
LOGGER = getLogger("Acssim.Corba.KnownAcsTypes")
#------------------------------------------------------------------------------
def getKnownBaciType(ifr_id):
    '''
    Returns an instance of an ACS BACI type.
    
    Params: ifr_id interface repository ID of the type
    
    Returns: an instance of the class defined by ifr_id
    
    Raises: an exception if this type has not been implemented
    by ACS.
    '''
    if ifr_id == "IDL:alma/ACSErr/Completion:1.0":
        return ACSErr.Completion(long(getTimeStamp().value),  #ULL;
                                 0L,  #ACSErr::CompletionType type;
                                 0L,  #ACSErr::CompletionCode code;
                                 ())
    else:
        raise "dummie exception"

#------------------------------------------------------------------------------
def tryCallbackParams(params, compRef):
    '''
    Takes in some parameters that were passed to a component method and checks
    to see if any of them are actually callbacks. If this is the case, the
    callbacks done method is invoked.
    '''
    from Acssim.Corba.Generator import getRandomValue
    
    compl = ACSErr.Completion(long(getTimeStamp().value),  #ULL
                              0L,  #ACSErr::CompletionType type;
                              0L,  #ACSErr::CompletionCode code;
                              ())
                              
    cbId = 0L    #default value
    for param in params:
        if isinstance(param, ACS.CBDescIn):
            cbId = param.id_tag
            break
        
    cbDescOut = ACS.CBDescOut(0L, cbId)

    for param in params:
        if isinstance(param, ACS._objref_CBvoid):
            param.done(compl,
                       cbDescOut)
        elif isinstance(param, ACS._objref_CBpattern):
            param.done(getRandomValue(ACS._tc_pattern, compRef),
                       compl,
                       cbDescOut)
        elif isinstance(param, ACS._objref_CBpattern):
            param.done(getRandomValue(ACS._tc_pattern, compRef),
                       compl,
                       cbDescOut)
        elif isinstance(param, ACS._objref_CBdouble):
            param.done(getRandomValue(CORBA._tc_double, compRef),
                       compl,
                       cbDescOut)
        elif isinstance(param, ACS._objref_CBstring):
            param.done(getRandomValue(CORBA._tc_string, compRef),
                       compl,
                       cbDescOut)
        elif isinstance(param, ACS._objref_CBstringSeq):
            param.done(getRandomValue(ACS._tc_stringSeq, compRef),
                       compl,
                       cbDescOut)
        elif isinstance(param, ACS._objref_CBlong):
            param.done(getRandomValue(CORBA._tc_long, compRef),
                       compl,
                       cbDescOut)
        elif isinstance(param, ACS._objref_CBdoubleSeq):
            param.done(getRandomValue(ACS._tc_doubleSeq, compRef),
                       compl,
                       cbDescOut)
        elif isinstance(param, ACS._objref_CBlongSeq):
            param.done(getRandomValue(ACS._tc_longSeq, compRef),
                       compl,
                       cbDescOut)
        else:
            pass

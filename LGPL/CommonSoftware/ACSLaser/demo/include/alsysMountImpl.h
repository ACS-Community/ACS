#ifndef alsysMountImpl_h
#define alsysMountImpl_h
/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

///Contains the defintion of the standard superclass for C++ components
#include <acscomponentImpl.h>

///CORBA generated servant stub
#include <demoComponentsS.h>

class Mount: public acscomponent::ACSComponentImpl,     
			     public virtual POA_alarmsystemdemo::Mount
{
  public:
     /**
     * Constructor
     */
    Mount(const ACE_CString &name,maci::ContainerServices * containerServices);
    
    /**
     * Destructor
     */
    virtual ~Mount();
    
    void faultMount();
                                                                                                                       
    void terminate_faultMount() ;
    
    /**
     * Send an alarm active or inactive depending on the
     * value of the parameter
     * 
     * 
     */
    void sendAlarm(std::string fFamily, std::string fMember, int code, bool active);

};

#endif /*!alsysMountImpl_H*/




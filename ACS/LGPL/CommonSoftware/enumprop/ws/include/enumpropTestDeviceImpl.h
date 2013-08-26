/* @(#) $Id: enumpropTestDeviceImpl.h,v 1.41 2009/10/02 14:00:28 bjeram Exp $
 */
/*
* DeviceImpl.h - ALMA Device interface implementation.
*    ALMA - Atacama Large Millimiter Array
*    (c) Associated Universities Inc., 2001
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
#ifndef _ENUM_PROP_TEST_DEVICE_IMPL_H_
#define _ENUM_PROP_TEST_DEVICE_IMPL_H_


#include <acsutil.h>

#include <baci.h>
#include "enumpropTestDeviceS.h"
#include "enumpropROImpl.h"
#include "enumpropRWImpl.h"
#include "baciCharacteristicComponentImpl.h"

NAMESPACE_USE(baci)
NAMESPACE_USE(ENUMPROP_TEST)

class  enumpropTestDeviceImpl : public POA_ENUMPROP_TEST::enumpropTestDevice,
				public baci::CharacteristicComponentImpl
  {
  public:
    //Constructor 
    enumpropTestDeviceImpl (
        const ACE_CString& name,
        maci::ContainerServices* containerServices);

    //Destructor 
    virtual ~enumpropTestDeviceImpl(void);

    /* ----------------------------------------------------------------*/
    /* --------------------- [ CORBA interface ] ----------------------*/
    /* ----------------------------------------------------------------*/
    ENUMPROP_TEST::ROStates_ptr currentState ();

    /*
    * @throw ENUMPROP_TEST::StateMachine::NoSuchTransition
    */
    virtual ACSErr::Completion * enable ();

    /*
    * @throw ENUMPROP_TEST::StateMachine::NoSuchTransition
    */
    virtual ACSErr::Completion * disable ();
  
    /*
    * @throw ENUMPROP_TEST::StateMachine::NoSuchTransition
    */
    virtual ACSErr::Completion * diagnose ();

    /*
    * @throw ENUMPROP_TEST::StateMachine::NoSuchTransition
    */
    virtual ACSErr::Completion * shutdown (); 
    
    /*
    * @throw ENUMPROP_TEST::StateMachine::NoSuchTransition
    */
    virtual ACSErr::Completion * init (); 

    /*
    * @throw ENUMPROP_TEST::StateMachine::NoSuchTransition
    */
    virtual ACSErr::Completion * on ();

    /*
    * @throw ENUMPROP_TEST::StateMachine::NoSuchTransition
    */
    virtual ACSErr::Completion * off ();

   ENUMPROP_TEST::RWStates_ptr currentStateRW (); 

      void serverShutdown ();

      /**
         * For test purpose we want to be able to change FF and FM
         */
      void changeAlarmFFFM(const char*, const char*);

  private:
    ROEnumImpl<ACS_ENUM_T(ENUMPROP_TEST::States), POA_ENUMPROP_TEST::ROStates> *m_currentState;
    RWEnumImpl<ACS_ENUM_T(ENUMPROP_TEST::States), POA_ENUMPROP_TEST::RWStates> *m_currentStateRW;     
  };


#endif 











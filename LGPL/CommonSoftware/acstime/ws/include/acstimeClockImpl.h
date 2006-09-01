/* @(#) $Id: acstimeClockImpl.h,v 1.15 2006/09/01 02:20:54 cparedes Exp $
 *
 * Copyright (C) 2001
 * Associated Universities, Inc. Washington DC, USA.
 *
 * Produced for the ALMA project
 *
 * This library is free software; you can redistribute it and/or modify it it 
 * under the terms of the GNU Library General Public License as published by 
 * the Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This library is distributed in the hope that it will be useful but WITHOUT 
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
 * License for more details.
 *
 * You should have received a copy of the GNU Library General Public License 
 * along with this library; if not, write to the Free Software Foundation, 
 * Inc., 675 Massachusetts Ave, Cambridge, MA, 02139, USA.
 *
 * Correspondence concerning ALMA should be addressed as follows:
 * Internet email: alma-sw-admin@nrao.edu
 */
////////////////////////////////////////////////////////////////////////
#ifndef ACSTIME_CLOCK_IMPL_H
#define ACSTIME_CLOCK_IMPL_H
////////////////////////////////////////////////////////////////////////
#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif
////////////////////////////////////////////////////////////////////////
#include <baciCharacteristicComponentImpl.h>
#include <baci.h>
#include <baciRWlong.h>
#include <baciROuLongLong.h>
#include <ace/Timer_Heap_T.h>
#include <ace/Timer_Queue_Adapters.h>
////////////////////////////////////////////////////////////////////////
#include "acstimeS.h"
#include "ACSTimeError.h"
#include "acstimeTimeUtil.h"
#include "acstimeDevIOTime.h"
////////////////////////////////////////////////////////////////////////
 using namespace baci;

/** @file acstimeClockImpl.h
 *  Header file for implementation of ACS Clock.
 */

/** 
 * @class ClockImpl
 * ClockImpl is the implementation of <a href="../../idl/html/interfaceacstime_1_1Clock.html">Clock IDL interface</a> and it 
 * provides an interface into the ACS time service which 
 * is based mainly on ALMA Control System software. It has the ability to 
 * do time conversions, return the current time, etc.  
 * There should be one Clock device running on each PC/LCU.
 *
 * TODO:
 * - use devIOs on array2TAI and TAI2UTC properties and make them read-only.
 *   This requires figuring out exactly who writes values out to these properties.
 * - provide way more time conversion methods in this class (and the IDL interface).
 *   Won't be terribly useful until this is done.
 */
class ClockImpl : public virtual baci::CharacteristicComponentImpl,
		  public virtual POA_acstime::Clock
{
  public:
    ////////////////////////////////////////////////////////////////////////
    /**
     * Constructor
     * @param poa Poa which will activate this and also all other components. 
     * @param name component's name. This is also the name that will be used to find the
     * configuration data for the component in the Configuration Database.
     * @param containerServices container services object
     */
    ClockImpl(
	      const ACE_CString &name,
	      maci::ContainerServices * containerServices);
    
    /**
     * Destructor
     */
    virtual ~ClockImpl();
    ////////////////////////////////////////////////////////////////////////
    /**
     * Implementation of IDL method.
     * Please see the documenation for the Clock IDL interface.
     * @htmlonly
       <li><a href="../../idl/html/interfaceacstime_1_1Clock.html">IDL Documentation</a></li><br><hr>
       @endhtmlonly
     */ 
    virtual acstime::Duration 
    getTimeInterval(const acstime::Epoch &prevEpoch)
	throw (CORBA::SystemException);
    
    /**
     * Implementation of IDL method.
     * Please see the documenation for the Clock IDL interface.
       @htmlonly
       <li><a href="../../idl/html/interfaceacstime_1_1Clock.html">IDL Documentation</a></li><br><hr>
       @endhtmlonly
     */ 
    virtual ACS::RWlong_ptr 
    array2TAI()
	throw (CORBA::SystemException);
    
    /**
     * Implementation of IDL method.
     * Please see the documenation for the Clock IDL interface.
     * @htmlonly
       <li><a href="../../idl/html/interfaceacstime_1_1Clock.html">IDL Documentation</a></li><br><hr>
       @endhtmlonly
     */ 
    virtual ACS::RWlong_ptr 
    TAI2UTC()
	throw (CORBA::SystemException);
    
    /**
     * Implementation of IDL method.
     * Please see the documenation for the Clock IDL interface.
     * @htmlonly
       <li><a href="../../idl/html/interfaceacstime_1_1Clock.html">IDL Documentation</a></li><br><hr>
       @endhtmlonly
     */    
    virtual ACS::ROuLongLong_ptr 
    now()
	throw (CORBA::SystemException);

    /**
     * Implementation of IDL method.
     * Please see the documenation for the Clock IDL interface.
     * @htmlonly
       <li><a href="../../idl/html/interfaceacstime_1_1Clock.html">IDL Documentation</a></li><br><hr>
       @endhtmlonly
     */ 
    virtual acstime::Epoch 
    fromISO8601(acstime::TimeSystem ts,
		const char *iso)
	throw (CORBA::SystemException, ACSTimeError::ArgErrorEx);
    
    /**
     * Implementation of IDL method.
     * Please see the documenation for the Clock IDL interface.
     * @htmlonly
       <li><a href="../../idl/html/interfaceacstime_1_1Clock.html">IDL Documentation</a></li><br><hr>
       @endhtmlonly
     */ 
    virtual char* 
    toISO8601(acstime::TimeSystem ts,
	      const acstime::Epoch &timeValue)
	throw (CORBA::SystemException, ACSTimeError::ArgErrorEx);

    ////////////////////////////////////////////////////////////////////////
  private:
    
    /**
     *  Implementation of the array2TAI IDL property.
     */
    RWlong *m_array2TAI;
    
    /**
     *  Implementation of the TAI2UTC IDL property.
     */
    RWlong *m_TAI2UTC;
    
    /**
     *  Implementation of the now IDL property (current time).
     */
    ROuLongLong *m_now;
    
    /**
     *  devIO used to get the current time used by the now property.
     */
    DevIOTime *m_now_dev;
    
    /// copy not allowed
    ClockImpl(const ClockImpl&);
    
    /// assignment not allowed
    void operator= (const ClockImpl&);
    ////////////////////////////////////////////////////////////////////////
};
#endif


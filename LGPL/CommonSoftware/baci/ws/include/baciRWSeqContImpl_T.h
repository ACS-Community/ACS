#ifndef _baciRWSeqContImpl_T_H_
#define _baciRWSeqContImpl_T_H_
/*******************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2003 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: baciRWSeqContImpl_T"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    13/02/2003  created
*/

/** 
 * @file 
 * Header file for BACI Read-write Sequence Cont. Property.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciRWcontImpl_T.h>

/**
 * Helper macro defines template parameters.
 */
#define ACS_RW_SEQ_T(T, TC) ACS::T##Seq*, ACS::CB##T##Seq, ACS::T##Seq##Seq, ACS::T##Seq##Seq_out, ACS::Monitor##T, baci::Monitor##T##Seq, ACS::T##Seq, TC, TC, POA_ACS::RW##T##Seq, const ACS::T##Seq&

namespace baci {

template <ACS_RW_C> 
class RWSeqContImpl : public virtual POA_SK,
		      public baci::RWcontImpl<ACS_RW_TL> 
{
  public:
    RWSeqContImpl(const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO=0, bool flagdeldevIO=false);
    
    /**
     * Set value method (value mutator) overriden from RWcont
     * To make RW property simetric to RO property
     * @param property property which requested value
     * @param value value to be returned
     * @param completion error handling structure
     * @param descOut callback descriptor
     */
    virtual void setValue(BACIProperty* property,
			  BACIValue* value, 
			  Completion &completion,
			  CBDescOut& descOut);
    
    /**
     * Implementation of async. increment method overridenf from RWcont
     */
    virtual ActionRequest incrementAction(BACIComponent* component_p, 
					  int callbackID,
					  const CBDescIn& descIn, 
					  BACIValue* value,
					  Completion& completion, 
					  CBDescOut& descOut);
    
    /**
     * Implementation of async. decrement method overriden from RWcont
     */
    virtual ActionRequest decrementAction(BACIComponent* component_p, 
					  int callbackID,
					  const CBDescIn& descIn, 
					  BACIValue* value,
					  Completion& completion, 
					  CBDescOut& descOut);
};

// #include "baciRWSeqContImpl_T.i"

 }; 

#endif



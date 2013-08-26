#ifndef _baciRWdiscImpl_T_H_
#define _baciRWdiscImpl_T_H_
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
* "@(#) $Id: baciRWdiscImpl_T"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    12/02/2003  created
*/

/** 
 * @file 
 * Header file for BACI Read-write Disc. Property Template Class.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciRWcommonImpl_T.h>

namespace baci {

template <ACS_RW_C> 
class RWdiscImpl : public virtual POA_SK,
		   public RWcommonImpl<ACS_RW_TL> 
{
  public:
    RWdiscImpl(const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO=0, bool flagdeldevIO=false,  bool initValue=true);
    // do we still need initalize ??
    virtual ~RWdiscImpl() {;}
};


 }; 

#endif



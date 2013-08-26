#ifndef _baciRWpattern_H_
#define _baciRWpattern_H_

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
* "@(#) $Id: baciRWpattern.h,v 1.105 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    2003/02/22  using templates for implementing RWpattern
* bjeram    2002/02/25  added support for DevIO
* msekoran  2001/03/09  modified
*/

/** 
 * @file 
 * Header file for BACI Read-write Pattern Property.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciPpatternImpl.h>
#include <baciRWdiscImpl_T.h>


namespace baci {

class RWpatternImpl;

/** @defgroup RWpatternTemplate RWpattern Class
 * The RWpattern class is a templated typedef so there is no actual inline doc generated for it per-se.
 *  @{
 * The RWpattern class is an implementation of the ACS::RWpattern IDL interface. See RWpatternImpl for the 
 * real methods.
 */
typedef RWpatternImpl RWpattern;
/** @} */

class baci_EXPORT RWpatternImpl : public virtual POA_ACS::RWpattern,
				  public RWdiscImpl<ACS_RW_T(pattern, ACS::pattern)>,
				  public PpatternImpl
{ 
  public:
    RWpatternImpl(const ACE_CString& name, BACIComponent *component_p, DevIO<ACS::pattern> *devIO=0, bool flagdeldevIO=false);
    ~RWpatternImpl();
};

 }; 

#endif  /* baciRWpattern */



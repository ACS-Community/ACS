#ifndef _baciROpattern_H_
#define _baciROpattern_H_

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
* "@(#) $Id: baciROpattern.h,v 1.104 2005/09/28 13:52:49 msekoran Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    2003/02/22  ROpattern is using templates
* bjeram    2002/11/29  changed to Monitorpattern 
* bjeram    2002/02/25  added support for DevIO 
* msekoran  2001/03/09  modified
*/

/** 
 * @file 
 * Header file for BACI Read-only Pattern Property.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciPpatternImpl.h>
#include <baciROdiscImpl_T.h>


NAMESPACE_BEGIN(baci);

class ROpatternImpl;

/** @defgroup ROpatternTemplate ROpattern Class
 * The ROpattern class is a templated typedef so there is no actual inline doc generated for it per-se.
 *  @{
 * The ROpattern class is an implementation of the ACS::ROpattern IDL interface. 
 * See ROpatternImpl for the real descriptions.
 */
typedef ROpatternImpl ROpattern;
/** @} */


class baci_EXPORT ROpatternImpl : public virtual POA_ACS::ROpattern,
				  public ROdiscImpl<ACS_RO_T(pattern, ACS::pattern)>,
				  public PpatternImpl
				  
{			 
 public:
    ROpatternImpl(const ACE_CString& name, BACIComponent *component_p, DevIO<ACS::pattern> *devIO=0, bool flagdeldevIO=false);
    ~ROpatternImpl();
};

NAMESPACE_END(baci);

#endif  /* baciROpattern */


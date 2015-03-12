/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration),
*    All rights reserved
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
*
* "@(#) $Id: baciROdouble.cpp,v 1.96 2009/09/15 08:52:12 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2003-01-24 removed all the code
* oat      2003/01/21 added templates for Monitors
* bgustafs 2001-07-12 removed warnings for unused arguments
* msekoran  2001/03/10  modified 
*/

#include "baciPcommonImpl_T.h"

namespace baci
{

//specialization for double
template<>
void CheckDevIOValue::checkValue<double>(double value)
{
	double absValue = fabs(value);
	if (absValue > 2.0e17 || (absValue > 0.0 && absValue < 1.0e-9))
	{
		ACS_LOG(LM_FULL_INFO ,"CheckDevIOValue::checkValue<double>", (LM_WARNING, "Value of property: %s (%f) read from DevIO has non meaningful physical value. Probably problem in DevIO impl!",
				propName_m.c_str(),
				value));
	}
}//checkMonitorValue<double>
};

/*___oOo___*/

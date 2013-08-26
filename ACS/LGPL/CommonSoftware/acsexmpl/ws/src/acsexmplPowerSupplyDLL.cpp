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
*
*
*
* "@(#) $Id: acsexmplPowerSupplyDLL.cpp,v 1.6 2004/01/15 01:45:37 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2003-02-26 
*/

/*
  Because classes will be derived from PowerSupply (most notably RampedPowerSupply), but
  we also want to be able to activate a PS object on it's own, we must separate the DLL 
  functions used by activator from the *Impl.cpp file.
 */

#include "acsexmplPowerSupplyImpl.h"

ACE_RCSID(acsexmpl, acsexmplPowerSupplyDLL, "$Id: acsexmplPowerSupplyDLL.cpp,v 1.6 2004/01/15 01:45:37 dfugate Exp $")
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(PowerSupply)

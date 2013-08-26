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
* "@(#) $Id: acsexmplPowerSupplyCurrentImpl.cpp,v 1.88 2006/10/19 09:47:40 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* gchiozzi 2002-03-04 Instead of writing in the CDB, now gets a reference to readback and calls set_sync().
* msekoran 2002-02-15 created
*/

#include <logging.h>
#include <acsexmplPowerSupplyCurrentImpl.h>

ACE_RCSID(acsexmpl, acsexmplPowerSupplyCurrentImpl, "$Id: acsexmplPowerSupplyCurrentImpl.cpp,v 1.88 2006/10/19 09:47:40 bjeram Exp $")
using namespace baci;

/////////////////////////////////////////////////
// PowerSupplyCurrent
/////////////////////////////////////////////////

/// just setting the member property to point to readback_p
/// also using the constructor inherited from RWdouble
PowerSupplyCurrent::PowerSupplyCurrent(const ACE_CString &name, 
				       BACIComponent *cob_p,
				       ROdouble *readback_p) :
/*    baci::PcommonImpl< double, ACS::CBdouble, ACS::doubleSeq, ACS::doubleSeq_out, ACS::Monitordouble, Monitordouble, double, double, double, POA_ACS::RWdouble>(name, cob_p, 0),*/
    baci::RWdouble(name, cob_p),
    m_readback_p(readback_p)
{
    ACS_TRACE("::PowerSupplyCurrent::PowerSupplyCurrent");   
}


/// async. set value action implementation
void 
PowerSupplyCurrent::setValue(BACIProperty *property_p,
			     BACIValue *value_p, 
			     Completion &completion,
			     CBDescOut &descOut)
{
    ACS::Time timestamp;
    
    /* Calls parent class method */
    baci::RWdouble::setValue(property_p, value_p, completion, descOut);
    
    /* Calls getDevIO()->writeDouble() for associated readback current */
    double value = value_p->doubleValue();
    m_readback_p->getDevIO()->write(value, timestamp);
}


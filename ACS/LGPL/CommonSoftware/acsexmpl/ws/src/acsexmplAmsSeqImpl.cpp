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
* "@(#) $Id: acsexmplAmsSeqImpl.cpp,v 1.112 2008/10/09 08:41:11 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-08-13 changed instances of getCOB() to m_cob
* david 2002-08-08 made SETCOEFF_ACTION a const static instead of a define
* naoj 2002-04-14 created 
*/

#include <acsexmplAmsSeqImpl.h>
#include <ACSErrTypeOK.h>

ACE_RCSID(acsexmpl, acsexmplAmsSeqImpl, "$Id: acsexmplAmsSeqImpl.cpp,v 1.112 2008/10/09 08:41:11 cparedes Exp $")
using namespace baci;

/////////////////////////////////////////////////
// AMS
/////////////////////////////////////////////////

AmsTestSeq::AmsTestSeq(
		       const ACE_CString &name,
		       maci::ContainerServices * containerServices) :
    CharacteristicComponentImpl(name, containerServices),
    m_RWdoubleSeqPM_p(0), 
    m_ROdoubleSeqPM_p(0)
{   
    ACS_TRACE("::AmsTestSeq::AmsTestSeq");
    
    // Create the Properties 
    // the property's name must be composed of the server's name and the
    //   property name.
    m_RWdoubleSeqPM_p  = new RWdoubleSeq(name+":RWdoubleSeqPM", getComponent());
    // if this property wasn't created properly, we destroy it and all 
    //   of this component's previous properties
    CHARACTERISTIC_COMPONENT_PROPERTY(RWdoubleSeqPM, m_RWdoubleSeqPM_p);

    m_ROdoubleSeqPM_p  = new ROdoubleSeq(name+":ROdoubleSeqPM", getComponent());
    CHARACTERISTIC_COMPONENT_PROPERTY(ROdoubleSeqPM, m_ROdoubleSeqPM_p);   
}
/////////////////////////////////////////////////
void
AmsTestSeq::execute()
{
    ACS::Time timestamp;
    
    // Set default values to properties
    // N.B. number of elements never mean to be fixed to 12, just an exmample.
    ACS::doubleSeq_var initialCoeffValue = new ACS::doubleSeq;
    initialCoeffValue->length(12);
    for( int i = 0 ; i < 12 ; i++)
	{
	initialCoeffValue[i]=static_cast<double>(i);
	}
    // set_sync is used to RW properties while getDevIO() must be used for RO properties
    m_RWdoubleSeqPM_p->set_sync(initialCoeffValue.in());
    m_ROdoubleSeqPM_p->getDevIO()->write(initialCoeffValue.in(), timestamp);
    
}
/////////////////////////////////////////////////

AmsTestSeq::~AmsTestSeq()
{
    ACS_TRACE("::AmsTestSeq::~AmsTestSeq");

    // This is in principle not necessary, since is done in cleanUp()
    // but we keep it here just to make sure it is done in any case,
    // also is cleanUp() is for any reason not called or if the
    // developer forgets to call the parent's class cleanUp().
    if (getComponent() != 0)
	{
	ACS_DEBUG_PARAM("::AmsTestSeq::~AmsTestSeq", "Destroying %s...", getComponent()->getName());
	}
   
    // properties
    if (m_RWdoubleSeqPM_p != 0) 
	{
	m_RWdoubleSeqPM_p->destroy();
	m_RWdoubleSeqPM_p=0;
	}
    if (m_ROdoubleSeqPM_p != 0) 
	{
	m_ROdoubleSeqPM_p->destroy();
	m_ROdoubleSeqPM_p=0;
	}
    
    ACS_DEBUG("::AmsTestSeq::~AmsTestSeq", "Properties destroyed");   
}
/* ----------------------------------------------------------------*/
/* --------------------- [ CORBA interface ] ----------------------*/
/* ----------------------------------------------------------------*/
void
AmsTestSeq::setCoeff ()
{
    ACS::Time timestamp;
    ACSErr::Completion_var completion;
    
    try
	{
	ACS_DEBUG_PARAM("::AmsTestSeq::setCoeffAction", "%s", getComponent()->getName());
	ACS_SHORT_LOG( ( LM_INFO, "setCoeffAction!" ) );
	
	/**
	 * Here m_RWdoubleSeqPM_p is actually set into m_ROdoubleSeqPM_p.
	 */
	/* Just synchronously reading the value of PM */
        ACS::doubleSeq_var valValue = m_RWdoubleSeqPM_p->get_sync(completion.out());
        if (valValue.ptr() == 0)
	    {
	    ACS_SHORT_LOG((LM_INFO,"acsexmplAmsSeq: .. null value returned."));
	    }
        else
	    {
	    ACS_SHORT_LOG((LM_INFO,"acsexmplAmsSeq: .. final value is (length %u):", valValue->length()));
	    m_ROdoubleSeqPM_p->getDevIO()->write(valValue.in(), timestamp); 
	    }
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"::AmsTestSeq::setCoeffAction"));
	}
}
/////////////////////////////////////////////////
ACS::RWdoubleSeq_ptr
AmsTestSeq::RWdoubleSeqPM ()
{
    if (m_RWdoubleSeqPM_p == 0)
	{
	return ACS::RWdoubleSeq::_nil();
	}
    
    ACS::RWdoubleSeq_var prop = ACS::RWdoubleSeq::_narrow(m_RWdoubleSeqPM_p->getCORBAReference());
    return prop._retn();
}
/////////////////////////////////////////////////
ACS::ROdoubleSeq_ptr
AmsTestSeq::ROdoubleSeqPM ()
{
    if (m_ROdoubleSeqPM_p == 0)
	{
	return ACS::ROdoubleSeq::_nil();
	}
    
    ACS::ROdoubleSeq_var prop = ACS::ROdoubleSeq::_narrow(m_ROdoubleSeqPM_p->getCORBAReference());
    return prop._retn();
}
/////////////////////////////////////////////////
// MACI DLL support functions
/////////////////////////////////////////////////
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(AmsTestSeq)




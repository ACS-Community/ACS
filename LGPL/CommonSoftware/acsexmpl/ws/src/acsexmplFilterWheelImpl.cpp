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
* "@(#) $Id: acsexmplFilterWheelImpl.cpp,v 1.12 2008/10/09 08:41:11 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2004-04-26 Creation
*/

#include <string>

#include <baciDB.h>
#include <maciContainerServices.h>
#include <acsexmplFilterWheelImpl.h>
#include <cdbDALC.h>

using namespace std;

/**
 * One of these function IDs will be passed to invokeAction().
 */
const static int MOVEFILTER_ACTION   = 0;
const static int MOVESLOT_ACTION   = 1;
const static int ADJUST_ACTION = 2;

ACE_RCSID(acsexmpl, acsexmplFilterWheelImpl, "$Id: acsexmplFilterWheelImpl.cpp,v 1.12 2008/10/09 08:41:11 cparedes Exp $")

using namespace baci;

/////////////////////////////////////////////////
// FilterWheel
/////////////////////////////////////////////////

FilterWheel::FilterWheel(const ACE_CString &name,maci::ContainerServices* containerServices) :
    CharacteristicComponentImpl(name,containerServices),
    m_position_sp(this),
    m_desc_sp(new ROstring(name+":desc", getComponent()),this),
    m_slots_sp(this), m_wheelConfiguration(0)
{
    // Build the full name of the component
    m_fullName="alma/"+name;

    ACS_TRACE("::FilterWheel::FilterWheel");
        
}

FilterWheel::~FilterWheel()
{
    ACS_TRACE("::FilterWheel::~FilterWheel");
}
/* --------------------- [ CDB related methods ] ---------------------*/
void FilterWheel::readConfiguration(Descriptor* descr)
{
    if (descr==NULL) {
    	ACS_SHORT_LOG((LM_ERROR,"FilterWheel::readConfiguration Uninitialized variable m_wheelConfiguration!"));
    	return;
    }
    
    // Get the number of available slots in the wheel from 
    // the property m_slots_sp
    // NOTE: this is also the length of the array of slots
    ACSErr::Completion_var completion;
    unsigned int availableSlots = (unsigned int)m_slots_sp->get_sync(completion);
    
    // Get the DAl from the container services
    CDB::DAL_ptr dal_p = getContainerServices()->getCDB();
    // Get tha DAo record for the component
    CDB::DAO_ptr dao_p = dal_p->get_DAO_Servant(m_fullName.c_str());

   
    // Read the sequence of long describing the position of each slot
    // in the wheel
    CDB::longSeq* lngSeq =dao_p->get_long_seq("SlotStep");
    
    // Check if the sequence of long is valid
    if (lngSeq==NULL) {
    	ACS_SHORT_LOG((LM_ERROR,"No slots found in %s",m_fullName.c_str()));
    	return;
    }
    
    // Check if the user defined all the slots
    if (lngSeq->length()!=availableSlots) {
    	ACS_SHORT_LOG((LM_WARNING,"Availbale and defined slots disagree in CDB"));
    }
    
   	for (CORBA::ULong t=0; t<lngSeq->length() && t<availableSlots; t++)
    {
   	    descr[t].step=(*lngSeq)[t];
    }
    
	// Read the keys of the filters
    // The key is the value of the first parameter in the CBD
    // For example in <_ Name="Red" Delta=..../> the key is Red.
	CDB::stringSeq* fltKeys = dao_p->get_string_seq("Filter");
    
    // Check if the sequence of keys is valid
    if (fltKeys==NULL) {
    	ACS_SHORT_LOG((LM_ERROR,"No slots found in %s",m_fullName.c_str()));
    	return;
    }
    // Read each filter definition and fill the wheel descriptor
    for (CORBA::ULong t=0; t<fltKeys->length() && t<availableSlots; t++) 
    {
    	char key[64];
    	char deltaName[128];
    	char slotName[128];
    	strcpy(key,((*fltKeys)[t]));
        // Define the name of the items to read
        // It is based on the name of the main tag (Filter) and the key
        // of the record (es. Red) plus the name of the item to read
        // (Delta or Slot in this example)
    	sprintf(deltaName,"Filter/%s/Delta",key);
    	sprintf(slotName,"Filter/%s/Slot",key);
        // Read the values from the CDB
    	int delta = dao_p->get_long(deltaName);
    	int slot = dao_p->get_long(slotName);
    	// Store the name and the delta in the right slot
    	if (slot>=0 && slot<(int)availableSlots) {
    		descr[slot].delta=delta;
    		strcpy(descr[slot].filterName,key);
    	} else {
    		ACS_SHORT_LOG((LM_ERROR,"The slot %d is invalid",slot));
    	}
    }
    
    for (unsigned int k=0; k<availableSlots; k++) 
    {
        std::cout<<k<<"> step="<<descr[k].step<<", delta="<<descr[k].delta;
        std::cout<<", filter=["<<descr[k].filterName<<"]\n";
    }
}

void FilterWheel::updateFilter(ACE_CString name, int delta)
{
    ACS_SHORT_LOG((LM_INFO,"Updating the CDB entry for the %s filter setting delta to %d",
		   name.c_str(), delta));
    // Get the DAl from the container services
    CDB::DAL_ptr dal_p = getContainerServices()->getCDB();
    // Get tha DAO record for the component
    //CDB::DAO_ptr dao_p = dal_p->get_DAO_Servant(m_fullName.c_str()); 

    CDB::WDAL_ptr wdal_p = CDB::WDAL::_narrow(dal_p);
    CDB::WDAO_ptr wdao_p = wdal_p->get_WDAO_Servant(m_fullName.c_str());
    

    // Update the entry on the CDB
    // We need to build the key as we did to red the values
    ACE_CString deltaStr("Filter/");
    deltaStr+=name;
    deltaStr+="/Delta";

    wdao_p->set_long(deltaStr.c_str(),delta);
}

void FilterWheel::updateWheel(int slot, int step)
{
    ACS_SHORT_LOG((LM_INFO,"Updating the position of slot %d to %d",slot,step));
    
    // Get the DAl from the container services
    CDB::DAL_ptr dal_p = getContainerServices()->getCDB();
    // Get tha DAO record for the component
    //CDB::DAO_ptr dao_p = dal_p->get_DAO_Servant(m_fullName.c_str()); 

    CDB::WDAL_ptr wdal_p = CDB::WDAL::_narrow(dal_p);
    CDB::WDAO_ptr wdao_p = wdal_p->get_WDAO_Servant(m_fullName.c_str());
    
    // Read the sequence of long describing the position of each slot
    // in the wheel from the CDB
    CDB::longSeq* lngSeq =wdao_p->get_long_seq("SlotStep");
    
    // Check if the sequence of long is valid
    if (lngSeq==NULL) {
      ACS_SHORT_LOG((LM_ERROR,"No slots found in %s",m_fullName.c_str()));
      return;
    }
    
    // Update the slot in the sequence
    (*lngSeq)[(CORBA::ULong)slot]=step;
    
    for (CORBA::ULong t=0; t<lngSeq->length(); t++)
    {
        std::cout<<t<<". "<<(*lngSeq)[t]<<"\n";
    }
    
    // Update the entry in the CDB
    wdao_p->set_long_seq("SlotStep", *lngSeq);
}

/* --------------------- [ Support methods ] -------------------- */


/* --------------------- [ Life cycle methods ] -------------------- */

 void FilterWheel::initialize()
{
    ACS_TRACE("::FilterWheel::initialize");

    m_position_sp=new ROdouble(getContainerServices()->getName()+":position", getComponent());
    // Position is 0 at the beginning
    ACS::Time timestamp;
    m_position_sp->getDevIO()->write(0.0, timestamp);

    // There are two ways to get an attribute of the component
    // 1. from the DAO of the component 
    //    (shown for the FilterWheel attribute)
    // 2. using the get_characteristic method
    //    (shown for the AvailableSlots attribute)
    // We'll show boths

    // 1
    // Get the description of this filter wheel from the xml file
    // It is an attribute of the component
    CDB::DAL_ptr dal_p = getContainerServices()->getCDB();
    //DAOImpl* dao_p = new DAOImpl(dal_p->get_DAO(m_fullName.c_str()));
    CDB::DAO_ptr dao_p = dal_p->get_DAO_Servant(m_fullName.c_str());
    char* descr=NULL;
    try 
	{
	descr = dao_p->get_string("FilterWheelDescription");
	if (descr!=NULL) 
	    {
	    ACS_SHORT_LOG((LM_INFO,"FilterWheel Description is %s",descr));
	    }
	else 
	    {
	    ACS_SHORT_LOG((LM_ERROR,"Descr is NULL!"));
	    }
	} 
    catch (...) 
	{
	   // We receive an error from the CDB 
	   // maybe a WrongDataType or a FieldDoesNotExist exception
	   descr=NULL;
	   ACS_SHORT_LOG((LM_ERROR,"Error reading FilterWheel Description from the CDB"));
	}
    if (descr!=NULL) 
    {
    	m_desc_sp->getDevIO()->write(descr, timestamp);
    }

    // 2
    // Get the number of available slots from the CDB
    // It is an attribute of the component
    m_slots_sp=new ROlong(getContainerServices()->getName()+":slots", getComponent());
    long availSlots;
    try 
	{
	CORBA::Any* characteristic = get_characteristic_by_name("AvailableSlots");
	const char* val;
	if (!(*characteristic>>=val))
	    {
	    ACS_SHORT_LOG((LM_ERROR,"Error getting AvailableSlots value by the CORBA::Any object"));
	    }
	else
	    {
	    // Convert the string
	    availSlots=atoi(val);
	    m_slots_sp->getDevIO()->write(availSlots, timestamp);
	    }
	}
    catch (...)
	{
	// An error occurred reading the characteristic
	// This method throws ACS::NoSuchCharacteristic
	ACS_SHORT_LOG((LM_ERROR,"Error reading the characteristic AvailableSlots by its name"));
	return;
	}
    
    // Build the array of descriptors
	if (availSlots>0) {
    	m_wheelConfiguration = new Descriptor[availSlots];
    	// Fill each item with default values
        for (int t=0; t<availSlots; t++) {
	    	m_wheelConfiguration[t].step=0;
	    	m_wheelConfiguration[t].delta=0;
	    	m_wheelConfiguration[t].filterName[0]=0;
    	}
	}
}

 void FilterWheel::execute() 
{
    ACS_TRACE("::FilterWheel::execute");

	// Read the CDb to build the actual configuration of the wheel
    readConfiguration(m_wheelConfiguration);
}

void FilterWheel::cleanUp()
{
    // Free the array
    delete[] m_wheelConfiguration;

}

void FilterWheel::aboutToAbort()
{
}

/* --------------- [ Action implementator interface ] -------------- */

ActionRequest 
FilterWheel::invokeAction (int function,
		    BACIComponent *cob_p, 
		    const int &callbackID, 
		    const CBDescIn &descIn, 
		    BACIValue *value_p, 
		    Completion &completion, 
		    CBDescOut &descOut) 
{
    ACS_SHORT_LOG((LM_INFO,"FilterWheel::invokeAction"));
    
    // better implementation with array is possible
    switch (function) 
	{
	case MOVEFILTER_ACTION:
	{
		return moveFilterAction(cob_p, callbackID, descIn, value_p, completion, descOut);
	}
    case MOVESLOT_ACTION:
    {
        return moveSlotAction(cob_p, callbackID, descIn, value_p, completion, descOut);
    }
	case ADJUST_ACTION:
	{
		return adjustAction(cob_p, callbackID, descIn, value_p, completion, descOut);
	}
	default:
	{
		ACS_SHORT_LOG((LM_WARNING,"Unimplemented action requested: %d is unknon",function));
		return reqDestroy;
	}
	}
}

/* ------------------ [ Action implementations ] ----------------- */

/// implementation of async. adjust() method
ActionRequest 
FilterWheel::adjustAction (
    BACIComponent *cob_p, 
    const int &callbackID,
    const CBDescIn &descIn, 
    BACIValue *value_p,
    Completion &completion, 
    CBDescOut &descOut)
{
    ACS_SHORT_LOG((LM_INFO,"::FilterWheel::adjustAction", "%s", getComponent()->getName()));

    int* steps = static_cast<int*>(const_cast<void *>(value_p->pointerValue()));

    ACS::Time timestamp;
    ACSErr::Completion_var compl;
    
    int newPosition=(int)m_position_sp->get_sync(compl)+*steps;
    m_position_sp->getDevIO()->write(newPosition, timestamp);
    ACS_SHORT_LOG((LM_INFO,"Wheel rotaed to step %d",newPosition));

    DBConnector::writeCommand(getComponent()->getName(), "move", getStringifiedTimeStamp());
    
    completion = ACSErrTypeOK::ACSErrOKCompletion();

    // if OK action will be destroyed and we do not need it anymore
    if (steps!=0) 
        {
        delete steps;
	steps=0;
        }
    
    // complete action requesting done invocation, 
    // otherwise return reqInvokeWorking and set descOut.estimated_timeout
    return reqInvokeDone;
}

/// implementation of async. move() method
ActionRequest 
FilterWheel::moveFilterAction (
    BACIComponent *cob_p, 
    const int &callbackID,
    const CBDescIn &descIn, 
    BACIValue *value_p,
    Completion &completion, 
    CBDescOut &descOut)
{
    ACS_SHORT_LOG((LM_INFO,"::FilterWheel::moveFilterAction", "%s", getComponent()->getName()));

    ACE_CString* name = static_cast<ACE_CString*>(const_cast<void *>(value_p->pointerValue()));
    
    ACSErr::Completion_var tempCompletion;
    unsigned int availableSlots = (unsigned int)m_slots_sp->get_sync(tempCompletion);

    // The user wish to move the wheel to a defined filter.
    // We look for the slot that contains the filters to get its step
    // its adjustment (step)
	bool found=false;
	unsigned int t=0;
	for (; t<availableSlots; t++) {
		if (strcmp(name->c_str(),m_wheelConfiguration[t].filterName)==0) {
			found=true;
			break;
		}
	}
	
	if (found) {
	    int delta=m_wheelConfiguration[t].delta;
	    unsigned int step=m_wheelConfiguration[t].step;
	    ACS::Time timestamp;
		m_position_sp->getDevIO()->write(step+delta, timestamp);
		ACS_SHORT_LOG((LM_INFO,"Wheel rotaed to step %d",step+delta));
	}
    else
	{
		ACS_SHORT_LOG((LM_ERROR,"No filters of type %s found",name->c_str()));
	}
    
    DBConnector::writeCommand(getComponent()->getName(), "move", getStringifiedTimeStamp());
    
    completion = ACSErrTypeOK::ACSErrOKCompletion();

    // if OK action will be destroyed and we do not need it anymore
    if (name!=0) 
        {
        delete name;
	name=0;
        }
    
    // complete action requesting done invocation, 
    // otherwise return reqInvokeWorking and set descOut.estimated_timeout
    return reqInvokeDone;
}

ActionRequest 
FilterWheel::moveSlotAction (
    BACIComponent *cob_p, 
    const int &callbackID,
    const CBDescIn &descIn, 
    BACIValue *value_p,
    Completion &completion, 
    CBDescOut &descOut)
{
    ACS_SHORT_LOG((LM_INFO,"::FilterWheel::moveSlotAction", "%s", getComponent()->getName()));
    
    int* slot = static_cast<int*>(const_cast<void *>(value_p->pointerValue()));

    ACS::Time timestamp;
    ACSErr::Completion_var compl;
    
    ACSErr::Completion_var tempCompletion;
    unsigned int availableSlots = (unsigned int)m_slots_sp->get_sync(tempCompletion);
    if (*slot>=0 && *slot<(int)availableSlots)
    {
        m_position_sp->getDevIO()->write(m_wheelConfiguration[*slot].step, timestamp);
        ACS_SHORT_LOG((LM_INFO,"Wheel rotaed to step %d",m_wheelConfiguration[*slot].step));
    }
    else 
    {
        ACS_SHORT_LOG((LM_ERROR,"Invalid slot %d",*slot));
    }

    DBConnector::writeCommand(getComponent()->getName(), "move", getStringifiedTimeStamp());
    
    completion = ACSErrTypeOK::ACSErrOKCompletion();

    // if OK action will be destroyed and we do not need it anymore
    if (slot!=0) 
    {
        delete slot;
        slot=0;
    }
    
    // complete action requesting done invocation, 
    // otherwise return reqInvokeWorking and set descOut.estimated_timeout
    return reqInvokeDone;
}

/* --------------------- [ CORBA interface ] ----------------------*/

void FilterWheel::moveFilterInBeam(const char* name, ACS::CBvoid_ptr cb,  const ACS::CBDescIn& desc) 
{
    ACE_CString* str = new ACE_CString(name);

    ACS_SHORT_LOG((LM_INFO,"FilterWheel::Move the filter %s in the beam",name));
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, MOVEFILTER_ACTION, BACIValue(str));
}

void FilterWheel::moveSlotInBeam(int slot, ACS::CBvoid_ptr cb, const ACS::CBDescIn& desc)
{
    int* slot_p= new int(slot);
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, MOVESLOT_ACTION, BACIValue(slot_p));
}

void FilterWheel::adjust(int step, ACS::CBvoid_ptr cb, const ACS::CBDescIn& desc)
{
    int* step_p= new int(step);
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ADJUST_ACTION, BACIValue(step_p));
}

CORBA::Long FilterWheel::calibrateWheel(int slot)
{
	// Get the number of slots
	ACSErr::Completion_var completion;
    int availableSlots = (int)m_slots_sp->get_sync(completion);
    
    if (slot>=0 && slot<availableSlots) {
    	m_wheelConfiguration[slot].step=(long)m_position_sp->get_sync(completion);
        updateWheel(slot,m_wheelConfiguration[slot].step);
    	ACS_SHORT_LOG((LM_INFO,"New step for slot %d is %d",slot,m_wheelConfiguration[slot].step));
    	return m_wheelConfiguration[slot].step;
    } else {
    	ACS_SHORT_LOG((LM_ERROR,"Invalid slot %d",slot));
    }
    return 0;
}

CORBA::Long FilterWheel::calibrateFilter(const char* name)
{
	// Get the number of slots
	ACSErr::Completion_var completion;
    unsigned int availableSlots = (unsigned int)m_slots_sp->get_sync(completion);
    
    long actualPosition = (long)m_position_sp->get_sync(completion);
    long delta;
	
	// Look for the filter to calibrate
	bool found=false;
	unsigned int t=0;
	for (; t<availableSlots; t++) {
		if (strcmp(name,m_wheelConfiguration[t].filterName)==0) {
			found=true;
			break;
		}
	}
	
	if (found) {
		// Delta is given by the difference between the actual position
		// and step for the slot containing the filter
		long step=m_wheelConfiguration[t].step;
		m_wheelConfiguration[t].delta=actualPosition-step;
        updateFilter(name, m_wheelConfiguration[t].delta);
		ACS_SHORT_LOG((LM_INFO,"New delta for %s is %d",name,m_wheelConfiguration[t].delta));
	} else {
		ACS_SHORT_LOG((LM_ERROR,"No filters of type %s found",name));
	}

    return delta;
}

ACS::ROdouble_ptr
FilterWheel::position ()
{
    if (m_position_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}

    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_position_sp->getCORBAReference());
    return prop._retn();
}

ACS::ROstring_ptr
FilterWheel::desc ()
{
    if (m_desc_sp == 0)
	{
	return ACS::ROstring::_nil();
	}

    ACS::ROstring_var prop = ACS::ROstring::_narrow(m_desc_sp->getCORBAReference());
    return prop._retn();
}

ACS::ROlong_ptr
FilterWheel::slots ()
{
    if (m_slots_sp == 0)
	{
	return ACS::ROlong::_nil();
	}

    ACS::ROlong_var prop = ACS::ROlong::_narrow(m_slots_sp->getCORBAReference());
    return prop._retn();
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(FilterWheel)
/* ----------------------------------------------------------------*/


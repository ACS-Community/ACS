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
* "@(#) $Id: acsexmplLampWheelImpl.cpp,v 1.21 2008/10/09 08:41:11 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2004-04-26 Creation
*/


#include <baciDB.h>
#include <maciContainerServices.h>
#include <acsexmplLampWheelImpl.h>
#include <expat.h>

/**
 * One of these function IDs will be passed to invokeAction().
 */
const static int MOVE_ACTION  = 0;

ACE_RCSID(acsexmpl, acsexmplLampWheelImpl, "$Id: acsexmplLampWheelImpl.cpp,v 1.21 2008/10/09 08:41:11 cparedes Exp $")

using namespace baci;

/////////////////////////////////////////////////
// LampWheel
/////////////////////////////////////////////////

LampWheel::LampWheel(
		     const ACE_CString &name,
		     maci::ContainerServices * containerServices) :
    CharacteristicComponentImpl(name,containerServices),
    m_position_sp(this),
    m_desc_sp(new ROstring(name+":desc", getComponent()),this),
    m_slots_sp(this)
{
    // Build the full name of the component
    m_fullName="alma/"+name;

    ACS_TRACE("::LampWheel::LampWheel");
        
}

LampWheel::~LampWheel()
{
    
    ACS_TRACE("::LampWheel::~LampWheel");
    
}

/* --------------------- [CDB related methods ] -------------------- */

// We need to rememeber where the parser is when a char section is found
// in order to store the value of each element in the right field of the
// struct
#define NO_TAG    0 // We are not parsing a tag
#define UNDEF_TAG 0 // We are parsing a tag we do not care about
#define SLOT_TAG  1 // We are parsing the Slot tag
#define LAMP_TAG  2 // We are parsing the Lamp tag
#define WUT_TAG   3 // We are parsing the WarmUpTime tag
#define WATT_TAG  4 // We are parsing the Watt tag
#define POS_TAG   5 // We are parsing the Position tag

// The libexpat allows to pass a user defined data struct between the call back
// to avoid using global variables.
typedef struct {
    // We fill the field of this slot while parsing
    // This slot is stored into the list when the end tag
    // is met (</Slot>)
    // While some fields are optional, they are filled at the beginning
    // with default values
    SlotDescriptor slotDescr;
    // The tag we're parsing
    short actualTag;
    // The list of slots (it is a pointer to m_lampWheelConfiguration)
    std::list<SlotDescriptor>* wheelDescriptor;
} ParserStruct;

/**
 * Read the configuration from the CDB parsing the xml file of the component
 * It uses expat to parse the file.
 * ACS library allows to read all the fields of each defined component property
 * but it is not a xml parser i.e. we need to parse the file by ourself.
 * If something went wrong while parsing the file, or setting up the parser
 * the method exits after logging a message.
 * It is not a real wheel of course so I did not take care of all the possible
 * errors like for example inconsistency, duplicate slots definition in the
 * CDB and so on... 
 *
 *@param config is the list to store the slots into
 *@return the number of slots inserted into the list
*/
int  LampWheel::retrieveConfigurationFromCDB(std::list<SlotDescriptor>& config)
{
    // Clear the list
    config.clear();

    // We need the ContainerServices in order to retrieve the DAL
    maci::ContainerServices* services_p=getContainerServices();
    if (services_p==NULL) 
	{
	ACS_SHORT_LOG((LM_ERROR,"Error getting the ContainerServices"));
	return 0;
	}

    // Get the DAL from the container services
    CDB::DAL_ptr dal_p = services_p->getCDB();
    if (dal_p==NULL) 
	{
	ACS_SHORT_LOG((LM_ERROR,"Error getting the CDB from the ContainerServices"));
	return 0;
	}

    // dao contains the xml
    char* dao = NULL;
    try
	{
	// Stores the XML into the dao string
	dao = dal_p->get_DAO(m_fullName.c_str());
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"Error getting DAO for %s",m_fullName.c_str()));
	return 0;
	}

    // The following line prints the whole xml file
    //std::cout << "DAO is: ["<< dao <<']'<<std::endl;

    // Parse the xml using expat
    // Please have a look on the libexpat documentation
    // for further details (http:://libexpat.souceforge.net)
    XML_Parser p = XML_ParserCreate(NULL);
    if (! p) 
	{
	ACS_SHORT_LOG((LM_ERROR,"Could not allocate memory for the parser!"));
	return 0;
	}

    //Connect to the parser the handler for the end and the start of a tag
    XML_SetElementHandler(p, start_hndl, end_hndl);

    // Connect the char handler
    XML_SetCharacterDataHandler(p,char_hndl);

    // A SlotDescriptor data struct is passed throurg the handlers
    // While parsing all the fields of the following struct will be filled
    // The only field that remain immutable is the pointer to the list
    // of slots
    ParserStruct commonData;
    commonData.wheelDescriptor=&config; 
    XML_SetUserData(p,&commonData);

    // We have all the xml in the string so we parse all the document
    // with just one call
    if (XML_Parse(p,dao,strlen(dao),TRUE)==0)
	{
	ACS_SHORT_LOG((LM_ERROR,"Error parsing!"));
	config.clear();
	}
    else
	{
	ACS_SHORT_LOG((LM_INFO,"XML document parsed"));
	}

    // Release the memory used by the parser
    XML_ParserFree(p);

    return config.size();
}

/**
 * The handler for the start element
 *
 * @see libexpat documentation for further info
 */
void  LampWheel::start_hndl(void* data, const XML_Char* el, const XML_Char** attr)
{
    ParserStruct* ps =(ParserStruct*)data;

    // Which is the current tag?
    ps->actualTag=NO_TAG;
    if (strcmp(el,"Slot")==0) 
	{
	//Clean the struct for the new Slot the parser is beginning to read
	ps->slotDescr.num=0;
	ps->slotDescr.lampName[0]=0;
	ps->slotDescr.warmUpTime=0;
	ps->slotDescr.watt=0;
	ps->slotDescr.pos=0;

	ps->actualTag=SLOT_TAG;
	}
    else if (strcmp(el,"Lamp")==0) ps->actualTag=LAMP_TAG;
    else if (strcmp(el,"WarmUpTime")==0) ps->actualTag=WUT_TAG;
    else if (strcmp(el,"Watt")==0) ps->actualTag=WATT_TAG;
    else if (strcmp(el,"Position")==0) ps->actualTag=POS_TAG;
    else ps->actualTag=UNDEF_TAG; // We do not care of other cases
	
    // Scan the attribute and fill the fields of the descriptor
    // This example is quite easy because the XML has no duplicate names
    // for attributes ;-)
    for (int t=0; attr[t]; t+=2) 
	{
	if (strcmp(attr[t],"LampType")==0) strcpy(ps->slotDescr.lampName,attr[t+1]);
	else if (strcmp(attr[t],"SlotNumber")==0) ps->slotDescr.num=atoi(attr[t+1]);
	}
}

/**
 * The handler for the end element
 *
 * @see libexpat documentation for further info
 */
void  LampWheel::end_hndl(void *data, const XML_Char* el)
{
    ParserStruct* ps =(ParserStruct*)data;
    // Where are not processing a tag...
    ps->actualTag=NO_TAG;

    // The Slot tag is parsed: we got a full struct now!
    if (ps!=NULL && strcmp(el,"Slot")==0) 
	{
	// Insert the struct into the list
	ps->wheelDescriptor->push_back(ps->slotDescr);
	}
    // Where are not processing a tag...
    ps->actualTag=NO_TAG;
}

/**
 * The handler for the char element
 *
 * @see libexpat documentation for further info
 */
void LampWheel::char_hndl(void *data, const XML_Char *s, int len)
{
    ParserStruct* ps =(ParserStruct*)data;
    char temp[512];
    strncpy(temp,s,len);
    temp[len]=0;
    // We have to store the string in the right field as stated 
    // by the actualTag field
    //
    // XML schema definition guarantee that all the strings represent
    // numbers i.e. atoi cannot fail
    switch(ps->actualTag) {
    case WUT_TAG   : ps->slotDescr.warmUpTime=atoi(temp); break;
    case WATT_TAG  : ps->slotDescr.watt=atoi(temp); break;
    case POS_TAG : ps->slotDescr.pos=atoi(temp); break;
      default: break;
		   }
}

/* --------------------- [ Life cycle methods ] -------------------- */

 void LampWheel::initialize()
{
    ACS_TRACE("::LampWheel::initialize");

    m_position_sp=new ROdouble(getContainerServices()->getName()+":position", getComponent());
    // Position is 0 at the beginning
    ACS::Time timestamp;
    m_position_sp->getDevIO()->write(0.0, timestamp);

    // There are two ways to get an attribute of the component
    // 1. from the DAO of the component 
    //    (shown for the LampWheelDescription attribute)
    // 2. using the get_characteristic method
    //    (shown for the AvailableSlots attribute)
    // We'll show boths

    // 1
    // Get the description of this lamp wheel from the xml file
    // It is an attribute of the component
    CDB::DAL_ptr dal_p = getContainerServices()->getCDB();
    CDB::DAO_ptr dao_p = dal_p->get_DAO_Servant(m_fullName.c_str());
    char* descr=NULL;
    try 
	{
	descr = dao_p->get_string("LampWheelDescription");
	} 
    catch (...) 
	{
	// We receive an error from the CDB 
	// maybe a WrongDataType or a FieldDoesNotExist exception
	descr=NULL;
	ACS_SHORT_LOG((LM_ERROR,"Error reading LampWheel Description from the CDB"));
	}
    if (descr!=NULL) {
    	m_desc_sp->getDevIO()->write(descr, timestamp);
    } 

    // 2
    // Get the number of available slots from the CDB
    // It is an attribute of the component
    m_slots_sp=new ROlong(getContainerServices()->getName()+":slots", getComponent());
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
	    long availSlots=atoi(val);
	    m_slots_sp->getDevIO()->write(availSlots, timestamp);
	    }
	}
    catch (...)
	{
	// An error occurred reading the characteristic
	// This method throws ACS::NoSuchCharacteristic
	ACS_SHORT_LOG((LM_ERROR,"Error reading the characteristic AvailableSlots by its name"));
	}

}

 void LampWheel::execute() 
{
    ACS_TRACE("::LampWheel::execute");
    // Read the actual configuration from the CDB
    //
    // There is no meaning in reading the configuration from the CDB
    // if we had an error before but this is only an example...
    int size=retrieveConfigurationFromCDB(m_lampWheelConfiguration);
    ACS_SHORT_LOG((LM_ERROR,"%d slots found",size));

}

void LampWheel::cleanUp()
{
    // Empty the list
    m_lampWheelConfiguration.clear();

}

void LampWheel::aboutToAbort()
{
}

/* --------------- [ Action implementator interface ] -------------- */

ActionRequest 
LampWheel::invokeAction (int function,
		    BACIComponent *cob_p, 
		    const int &callbackID, 
		    const CBDescIn &descIn, 
		    BACIValue *value_p, 
		    Completion &completion, 
		    CBDescOut &descOut) 
{
    ACS_SHORT_LOG((LM_INFO,"LampWheel::invokeAction"));
    
    // better implementation with array is possible
    switch (function) 
	{
	case MOVE_ACTION:
	  {
	  ACS_SHORT_LOG((LM_INFO,"LampWheel::invokeAction call moveAction"));
	  return moveAction(cob_p, callbackID, descIn, value_p, completion, descOut);
	  }
	default:
	  {
	  return reqDestroy;
	  }
	}
}

/* ------------------ [ Action implementations ] ----------------- */


/// implementation of async. move() method
ActionRequest 
LampWheel::moveAction (BACIComponent *cob_p, 
		const int &callbackID,
		const CBDescIn &descIn, 
		BACIValue *value_p,
		Completion &completion, 
		CBDescOut &descOut)
{
    ACS_SHORT_LOG((LM_INFO,"::LampWheel::moveAction", "%s", getComponent()->getName()));

    int* slot_p = static_cast<int *>(const_cast<void *>(value_p->pointerValue()));

    // The user wish to move the wheel to a defined slot.
    // We look for the slot in the list of slots to retrieve the
    // position to rotate the wheel to
    std::list<SlotDescriptor>::iterator iter;
    bool found=false;
    for (iter=m_lampWheelConfiguration.begin(); iter!=m_lampWheelConfiguration.end(); ++iter) 
	{
	if ((int)iter->num==*slot_p) 
	    {
	    // The requested slot is on the list:
	    // the wheel rotates to the position specified in the CDB 
	    ACS::Time timestamp;
	    m_position_sp->getDevIO()->write(iter->pos, timestamp);
	    ACS_SHORT_LOG((LM_INFO,"Wheel moved to %d (using %s lamp, watt=%d)",
			   iter->pos,
			   iter->lampName,
			   iter->watt));
	    found=true;
	    break;
	    }
	}
    if (!found) 
	{
	ACS_SHORT_LOG((LM_ERROR,"The slot %d is not defined in the CDB",*slot_p));
	}
    
    DBConnector::writeCommand(getComponent()->getName(), "move", getStringifiedTimeStamp());
    
    completion = ACSErrTypeOK::ACSErrOKCompletion();

    // if OK action will be destroyed and we do not need it anymore
    if (slot_p!=0) 
        {
        delete slot_p;
	slot_p=0;
        }
    
    // complete action requesting done invocation, 
    // otherwise return reqInvokeWorking and set descOut.estimated_timeout
    return reqInvokeDone;
}

/* --------------------- [ CORBA interface ] ----------------------*/

void LampWheel::move(CORBA::Short slot, ACS::CBvoid_ptr cb,  const ACS::CBDescIn& desc) 
{
    int* param_p= new int;
    *param_p=slot;
    
    ACS_SHORT_LOG((LM_INFO,"LampWheel::Move to slot %d",*param_p));
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, MOVE_ACTION, BACIValue(param_p));
}

ACS::ROdouble_ptr
LampWheel::position ()
{
    if (m_position_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}

    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_position_sp->getCORBAReference());
    return prop._retn();
}

ACS::ROstring_ptr
LampWheel::desc ()
{
    if (m_desc_sp == 0)
	{
	return ACS::ROstring::_nil();
	}

    ACS::ROstring_var prop = ACS::ROstring::_narrow(m_desc_sp->getCORBAReference());
    return prop._retn();
}

ACS::ROlong_ptr
LampWheel::slots ()
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
MACI_DLL_SUPPORT_FUNCTIONS(LampWheel)
/* ----------------------------------------------------------------*/



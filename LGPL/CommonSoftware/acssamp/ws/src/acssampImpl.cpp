/*******************************************************************************
 *ALMA - Atacama Large Millimiter Array
 *(c) European Southern Observatory, 2003 
 *Copyright by ESO (in the framework of the ALMA collaboration),
 *All rights reserved
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
 * "@(#) $Id: acssampImpl.cpp,v 1.26 2005/04/14 09:41:05 acaproni Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * oat  2003-04-29  created 
 */

/** @file acssampImpl.cpp
 *  Source file for ACSSampImpl implementation.  
 */

#include <vltPort.h>

static char *rcsId="@(#) $Id: acssampImpl.cpp,v 1.26 2005/04/14 09:41:05 acaproni Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <Request.h>
#include "acssampImpl.h"
#include "acssampObjImpl.h"
#include <maciContainerImpl.h>
#include <maciACSComponentDefines.h>

using namespace baci;
using namespace maci;
using namespace ACSErrTypeCommon;


//
// ACSSampImpl Constructor
//
ACSSampImpl::ACSSampImpl(
			 const ACE_CString &name, 
			 maci::ContainerServices *containerServices) : 
    CharacteristicComponentImpl(name, containerServices)
{
    ACS_TRACE("ACSSamp::ACSSampImpl::ACSSampImpl");
}


//
// ACSSampImpl Destructor
//
ACSSampImpl::~ACSSampImpl()
{

    ACS_TRACE("ACSSamp::ACSSampImpl::~ACSSampImpl");

    ACS_DEBUG_PARAM("ACSSamp::ACSSampImpl::~ACSSampImpl","Destroying %s", getComponent()->getName());

    // delete all allocated object, that were not correctly deallocated
    if(!component_list.empty())
	{
	unsigned int max_size = component_list.size();
	for (unsigned int jj=0;jj<max_size;jj++)
	    {

	    list<CORBA::Object_ptr>::iterator i = component_list.begin();
	    
	    ACSSamp::SampObj_var mySamp = ACSSamp::SampObj::_narrow(*i);
	    if (!CORBA::is_nil(mySamp.in()))
		{
		mySamp->destroy();
		}
	    }
	}


    // stop threads
    if(getComponent() != 0)
	{
	getComponent()->stopAllThreads();
	}

}


//
// ACSSampImpl initSampObj method. Using the Dynamic Invokation
// interface (DII), gets the CORBA reference to the property to be sampled
// and discovers also its type (RWdouble etc.). This information
// is then used to correctly construct a new sampling object, which
// in turn allows to control the sampling.
//
ACSSamp::SampObj_ptr
ACSSampImpl::initSampObj(const char* name, const char* property, 
			 ACS::TimeInterval frequency, ACS::TimeInterval reportRate
			 )
    throw (CORBA::SystemException, OutOfBoundsEx,
	   MemoryFaultEx,CORBAProblemEx,TypeNotSupportedEx,
	   CouldntAccessPropertyEx,CouldntAccessComponentEx,
	   CouldntCreateObjectEx)

{
  
    ACS_TRACE("ACSSamp::ACSSampImpl::initSampObj");

    ACS_SHORT_LOG((LM_INFO,"Starting SampObj creation ... "));

    // discover with DII, Component properties
  
    ACSSamp::SampObj_var samp = ACSSamp::SampObj::_nil();

    ACE_CString cobName=CORBA::string_dup(name);
    ACE_CString propName=CORBA::string_dup(property);
    ACE_CString fullName=cobName+":"+propName;


    try
	{

	CORBA::Object_var obj =
	    ContainerImpl::getContainer()->get_object(name, 0, true);
      
	if (!CORBA::is_nil(obj.in()))
	    {

	    ACS_DEBUG("ACSSamp::ACSSampImpl::initSampObj","DII invocation");

	    CORBA::Request_var reqV;
	    reqV=obj->_request("descriptor");

	    ACS::CharacteristicComponentDesc *ret_struct;
	    reqV->set_return_type (ACS::_tc_CharacteristicComponentDesc);

	    reqV->invoke();
	  
	    reqV->return_value() >>= ret_struct;
	  
            // contains the number of properties for a specific component
	    CORBA::ULong len = ret_struct->properties.length();

            // internal index in the while loop; must not exceed the
            // number of properties
	    CORBA::ULong ind = 0;

            // error flag;
	    int errGuard = 1;

	    while (ind < len)
		{ 
         
                // Is the name of the discovered DII component 
                // the same, as what the user has request to sample?
                // if yes then we proceed, otherwise we throw an exception
		if (ACE_OS::strcmp(ret_struct->properties[ind].name,fullName.c_str()) == 0 )
		    {
		    ACS::Property_var cobProperty= ret_struct->properties[ind].property_ref;

		    /*
		     * !!!!!! GCH: We do not have get_interface any more !!!!!!
		     * Commented out for the time being:
		     *
		     * ACE_CString myPropIntf = CORBA::string_dup(cobProperty->get_interface());
		     * This shall be retrieved from the Component Info or from the IR
		     */

		    /******************************/
		    /*
		     * Other possibilities, but not working for our purposes
		     * 1) 
		     * ACE_CString myPropIntf = CORBA::string_dup(cobProperty ->_interface_repository_id());
		     * ACS_SHORT_LOG((LM_INFO,"NNNNNNNN %s",myPropIntf.c_str()));
		     *
		     * 2)
		     * CORBA::InterfaceDef_var intf_defV = cobProperty ->_get_interface();
		     * CORBA::InterfaceDef::FullInterfaceDescription_var full_descV = intf_defV->describe_interface();
		     * for (CORBA::ULong n=0; n< full_descV->operations.length();n++)
		     * {
		     *  cout <<  full_descV->id << endl;
		     * }
		     *
		     * 3)
		     * maci::HandleSeq seq;
		     * maci::ComponentInfoSeq_var cobs =
		     *       ContainerImpl::getContainer()->get_component_info(seq);
		     * for (CORBA::ULong i=0; i<cobs->length(); i++)
		     *  {
		     *   ACS_SHORT_LOG((LM_INFO,"%s (%s)", cobs[i].name.in(), cobs[i].type.in()));
		     *   for(CORBA::ULong j=0; j < cobs[i].interfaces.length();j++)
		     *   {
		     *           ACS_SHORT_LOG((LM_INFO,"%s ",cobs[i].interfaces[j].in()));
		     *  }
		     * }
		     */
		    /********************************/

		    CORBA::InterfaceDef_var intf_defV = cobProperty ->_get_interface();
		    CORBA::InterfaceDef::FullInterfaceDescription_var full_descV = intf_defV->describe_interface();
		    ACE_CString myPropIntf = CORBA::string_dup(full_descV->id);

                    // to be changed in future: this if...else if ... sequence simply
                    // selects the correct type; is certainly not the best way to do it,
                    // but for the time being is sufficient.

		    if ( myPropIntf.find("RWdouble") != -1) 
			{

			ACS_DEBUG("ACSSamp::ACSSampImpl::initSampObj","discovered RWdouble property type");
			ACS_DEBUG("ACSSamp::ACSSampImpl::initSampObj","DII invocation ended");

                        // dynamically create a new sampling object
			ACSSampObjImpl<ACS::RWdouble,ACS::RWdouble_var,CORBA::Double>* sampling = NULL;

			try
			    { 
			    sampling=new ACSSampObjImpl<ACS::RWdouble,ACS::RWdouble_var,CORBA::Double>(name,property,frequency,reportRate,getComponent(),cobProperty,this);
			    if (!sampling)
				{
				ACS_SHORT_LOG((LM_INFO,"Failed to allocate memory for the sampling object"));
				MemoryFaultExImpl err = 
				    MemoryFaultExImpl(__FILE__,__LINE__,"ACSSamp::ACSSampImpl::initSampObj");
				throw err.getMemoryFaultEx();
				}

			    sampling->initialize();

			    samp =  ACSSamp::SampObj::_narrow(sampling->getCORBAReference());
			    if (CORBA::is_nil(samp.in()))
				{
				ACS_SHORT_LOG((LM_INFO,"Failed to narrow the sampling object"));
				CORBAProblemExImpl err = 
				    CORBAProblemExImpl(__FILE__,__LINE__,"ACSSamp::ACSSampImpl::initSampObj");
				throw err;
				}
			    }
			catch(...)
			    {
			    if (sampling) 
				delete sampling;
                            // just re-throw
			    throw;
			    }
			
			} 
		    else if (myPropIntf.find("ROdouble") != -1)
			{

			ACS_DEBUG("ACSSamp::ACSSampImpl::initSampObj","discovered ROdouble property type");
			ACS_DEBUG("ACSSamp::ACSSampImpl::initSampObj","DII invocation ended");

			ACSSampObjImpl<ACS::ROdouble,ACS::ROdouble_var,CORBA::Double>* sampling = NULL;

			try
			    { 
			    sampling=new ACSSampObjImpl<ACS::ROdouble,ACS::ROdouble_var,CORBA::Double>(name,property,frequency,reportRate,getComponent(),cobProperty,this);
			    if (!sampling)
				{
				ACS_SHORT_LOG((LM_INFO,"Failed to allocate memory for the sampling object"));
				MemoryFaultExImpl err = 
				    MemoryFaultExImpl(__FILE__,__LINE__,"ACSSamp::ACSSampImpl::initSampObj");
				throw err;
				}

			    sampling->initialize();

			    samp =  ACSSamp::SampObj::_narrow(sampling->getCORBAReference());
			    if (CORBA::is_nil(samp.in()))
				{
				ACS_SHORT_LOG((LM_INFO,"Failed to narrow the sampling object"));
				CORBAProblemExImpl err = 
				    CORBAProblemExImpl(__FILE__,__LINE__,"ACSSamp::ACSSampImpl::initSampObj");
				throw err;
				}
			    }
			catch(...)
			    {
			    if (sampling) 
				delete sampling;
                            // just re-throw
			    throw;
			    }
		      
			} 
		    else if (myPropIntf.find("RWlong") != -1)
			{

			ACS_DEBUG("ACSSamp::ACSSampImpl::initSampObj","discovered RWlong property type");
			ACS_DEBUG("ACSSamp::ACSSampImpl::initSampObj","DII invocation ended");

			ACSSampObjImpl<ACS::RWlong,ACS::RWlong_var,CORBA::Long>* sampling = NULL;

			try
			    { 
			    sampling=new ACSSampObjImpl<ACS::RWlong,ACS::RWlong_var,CORBA::Long>(name,property,frequency,reportRate,getComponent(),cobProperty,this);
			    if (!sampling)
				{
				ACS_SHORT_LOG((LM_INFO,"Failed to allocate memory for the sampling object"));
				MemoryFaultExImpl err = 
				    MemoryFaultExImpl(__FILE__,__LINE__,"ACSSamp::ACSSampImpl::initSampObj");
				throw err;
				}

			    sampling->initialize();

			    samp =  ACSSamp::SampObj::_narrow(sampling->getCORBAReference());
			    if (CORBA::is_nil(samp.in()))
				{
				ACS_SHORT_LOG((LM_INFO,"Failed to narrow the sampling object"));
				CORBAProblemExImpl err = 
				    CORBAProblemExImpl(__FILE__,__LINE__,"ACSSamp::ACSSampImpl::initSampObj");
				throw err;
				}
			    }
			catch(...)
			    {
			    if (sampling) 
				delete sampling;
                            // just re-throw
			    throw;
			    }

			}
		    else if (myPropIntf.find("ROlong") != -1)
			{

			ACS_DEBUG("ACSSamp::ACSSampImpl::initSampObj","discovered ROlong property type");
			ACS_DEBUG("ACSSamp::ACSSampImpl::initSampObj","DII invocation ended");

			ACSSampObjImpl<ACS::ROlong,ACS::ROlong_var,CORBA::Long>* sampling = NULL;

			try
			    { 
			    sampling=new ACSSampObjImpl<ACS::ROlong,ACS::ROlong_var,CORBA::Long>(name,property,frequency,reportRate,getComponent(),cobProperty,this);
			    if (!sampling)
				{
				ACS_SHORT_LOG((LM_INFO,"Failed to allocate memory for the sampling object"));
				MemoryFaultExImpl err = 
				    MemoryFaultExImpl(__FILE__,__LINE__,"ACSSamp::ACSSampImpl::initSampObj");
				throw err;
				}

			    sampling->initialize();

			    samp =  ACSSamp::SampObj::_narrow(sampling->getCORBAReference());
			    if (CORBA::is_nil(samp.in()))
				{
				ACS_SHORT_LOG((LM_INFO,"Failed to narrow the sampling object"));
				CORBAProblemExImpl err = 
				    CORBAProblemExImpl(__FILE__,__LINE__,"ACSSamp::ACSSampImpl::initSampObj");
				throw err;
				}
			    }
			catch(...)
			    {
			    if (sampling)
				delete sampling;
                            // just re-throw
			    throw;
			    }

			}
		    else 
			{
			ACS_DEBUG("ACSSamp::ACSSampImpl::initSampObj","type not supported");
			TypeNotSupportedExImpl exc =
			    TypeNotSupportedExImpl(__FILE__,__LINE__,"ACSSamp::ACSSampImpl::initSampObj");
			exc.addData("Type Name",myPropIntf.c_str());
			throw exc;
			}

		    errGuard = 0;
		    break;
		    }

		ind++;
		}

	    if (errGuard)
		{
		ACS_DEBUG("ACSSamp::ACSSampImpl::initSampObj","property not found");
		CouldntAccessPropertyExImpl exc =
		    CouldntAccessPropertyExImpl(__FILE__,__LINE__,"ACSSamp::ACSSampImpl::initSampObj");
		exc.addData("Property Name",property);
		throw exc;
		}
    
	    } 
	else 
	    {
	    ACS_DEBUG("ACSSamp::ACSSampImpl::initSampObj","component not found");
	    CouldntAccessComponentExImpl exc =
		CouldntAccessComponentExImpl(__FILE__,__LINE__,"ACSSamp::ACSSampImpl::initSampObj");
	    exc.addData("Component Name",name);
	    throw exc;
	    }
	}
    catch (OutOfBoundsExImpl & exc)
	{
	ACS_SHORT_LOG((LM_INFO,"Catched by ACSSamp::ACSSampImpl OutOfBoundsExImpl"));
	throw;
	}
    catch (CouldntAccessComponentExImpl & exc)
	{
	ACS_SHORT_LOG((LM_INFO,"Catched by ACSSamp::ACSSampImpl CouldntAccessComponentEx"));
//	exc.addData("Rethrow","again");
	throw;
	}
    catch (CouldntAccessPropertyExImpl & exc)
	{
	ACS_SHORT_LOG((LM_INFO,"Catched by ACSSamp::ACSSampImpl CouldntAccessPropertyEx"));
	throw;
	}
    catch (CouldntCreateObjectExImpl & exc)
	{
	ACS_SHORT_LOG((LM_INFO,"Catched by ACSSamp::ACSSampImpl CouldntCreateObjectEx"));
	throw;
	}
   catch (TypeNotSupportedExImpl & exc)
	{
	ACS_SHORT_LOG((LM_INFO,"Catched by ACSSamp::ACSSampImpl TypeNotSupportedEx"));
	throw;
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_INFO,"Generic exception catched by ACSSamp::ACSSampImpl"));
	throw;
	}

    ACS_DEBUG("ACSSamp::ACSSampImpl::initSampObj","SampObj correctly created");
    ACS_SHORT_LOG((LM_INFO," ... SampObj correctly created!"));

    return samp._retn();

}


//
// ACSSampImpl addComponenttoList: internal method used to store reference
// to allocated sampling objects.
//
void
ACSSampImpl::addComponenttoList(CORBA::Object_ptr component_ref) 
{

    ThreadSyncGuard guard(&m_samplingListMutex);

    component_list.push_back(component_ref);
    ACS_DEBUG_PARAM("ACSSamp::ACSSampImpl::addComponenttoList","current number of SampObj: %d",component_list.size());

}


//
// ACSSampImpl addComponenttoList: internal method used to deallocate
// sampling objects, which were not correctly destroyed.
//
void
ACSSampImpl::removeComponentfromList(CORBA::Object_ptr component_ref) 
{

    ThreadSyncGuard guard(&m_samplingListMutex);

    component_list.remove(component_ref);
    ACS_DEBUG_PARAM("ACSSamp::ACSSampImpl::removeComponentfromList","remaining number of SampObj: %d",component_list.size());

}


/* --------------- [ MACI DLL support functions ] -----------------*/

MACI_DLL_SUPPORT_FUNCTIONS(ACSSampImpl)

/* ----------------------------------------------------------------*/



/*___oOo___*/


    






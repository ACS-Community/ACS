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
 * "@(#) $Id: "
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * acaproni  2005/12/07  created
 */
 
#include <logging.h>
#include <cdbDALC.h>
#include <cdbDALS.h>
#include <cdbDAONode.h> 
#include "cdbErrType.h"

#ifdef MAKE_VXWORKS
#	include "rebootLib.h"
#	include "acsutilArgUnpack.h"
int startNodeTest (char *szCmdLn)
{
    int  argc;
    char *argv[100];

    argc = argUnpack(szCmdLn, argv);
    argv[0] = "cdbTest";
#else
int main(int argc, char* argv[]) 
{
#endif // defined( MAKE_VXWORKS )

    LoggingProxy logger(0, 0, 31);
    
    // initialize the rest of LoggingProxy
    LoggingProxy::ProcessName(argv[0]);
    LoggingProxy::ThreadName("main");
    
    LoggingProxy::init (&logger); 
    
    ACS_SHORT_LOG ((LM_INFO, "nodeTest starts"));
    
    ACS_SHORT_LOG ((LM_INFO, "Init CORBA"));
    CORBA::ORB_var orb = CORBA::ORB_init(argc,argv);
    ACS_SHORT_LOG ((LM_INFO, "Getting RootPOA"));
    CORBA::Object_var obj = orb->resolve_initial_references("RootPOA");
    PortableServer::POA_var root_poa = PortableServer::POA::_narrow(obj);
    if (CORBA::is_nil(root_poa)) 
    {
    	ACS_SHORT_LOG ((LM_INFO, "The POA is NULL!"));
    	exit(-1);
    }
    else
    {
    	ACS_SHORT_LOG ((LM_INFO, "CORBA and POA initialized"));
    }
    

    ACS_SHORT_LOG ((LM_INFO, "Getting the DAL"));
    // Get the reference from the environment
    ACE_TCHAR * envRef = ACE_OS::getenv ("DAL_REFERENCE");
    if (envRef && *envRef)
	{
	ACS_SHORT_LOG((LM_INFO,"DALReference obtained via environment: '%s'", envRef));
	}
    else
	{
    	ACS_SHORT_LOG ((LM_INFO, "The DALReference is NULL! Set it in the environment!"));
    	exit(-1);
	}
    CORBA::Object_var dalObj = orb -> string_to_object(envRef);
    CDB::DAL_var dal = CDB::DAL::_narrow(dalObj);
    if (CORBA::is_nil(dal)) 
	{
    	ACS_SHORT_LOG ((LM_INFO, "The DAL is NULL!"));
    	exit(-1);
	}
    else
	{
    	ACS_SHORT_LOG ((LM_INFO, "The DAL is here"));
	}
    
    /////////////////////////////////////////////////////////////////////////////
    // The test starts here!
    /////////////////////////////////////////////////////////////////////////////
    
    // Get the first node to test long, double and string
    cdb::DAONode node("alma/TEST_PS_1",dal,root_poa);
    
    CORBA::Long lng = node.get_long("current/graph_max");
    ACS_SHORT_LOG ((LM_INFO, "current/graph_max = %ld",lng));
    
    CORBA::Double dbl = node.get_double("readback/alarm_low_off");
    ACS_SHORT_LOG ((LM_INFO, "readback/alarm_low_off = %lf",dbl));
    
    char* str = node.get_string("status/description");
    ACS_SHORT_LOG ((LM_INFO, "status/description = %s",str));
    
    // Get another node to test a squence of string
    cdb::DAONode managerNode("MACI/Managers/Manager",dal,root_poa);
    ::CDB::stringSeq* strSeq = managerNode.get_string_seq("ServiceComponents");
    CORBA::ULong length = strSeq->length();
    ACS_SHORT_LOG ((LM_INFO, "Found %ld strings: ",length));
    for (CORBA::ULong t=0; t<length; t++) 
    {
    	const char* temp = (strSeq->operator[](t));
    	ACS_SHORT_LOG ((LM_INFO, "%ld> %s",t,temp));
    }
    
    // Get another node to test sequences of long and doubles
    cdb::DAONode filteWheelNode("alma/FILTERWHEEL1",dal,root_poa);
    ::CDB::longSeq* lngSeq = filteWheelNode.get_long_seq("SlotStep");
    length = lngSeq->length();
    ACS_SHORT_LOG ((LM_INFO, "Found %ld long: ",length));
    for (CORBA::ULong t=0; t<length; t++) 
    {
    	long temp = (lngSeq->operator[](t));
    	ACS_SHORT_LOG ((LM_INFO, "%ld> %ld",t,temp));
    }
    // Get the attributes of position
    char* attrs = filteWheelNode.get_field_data("position");
    ACS_SHORT_LOG ((LM_INFO, "Attributes of position: %s",attrs));
    
    // Finally, try to build a non existent node
    try {
    	cdb::DAONode wrongNode("alma/DoesNotExist",dal,root_poa);
    	ACS_SHORT_LOG ((LM_ERROR, "Oh oh the test should not arrive here: the node does not exist!"));
    } 
    catch (cdbErrType::CDBRecordDoesNotExistExImpl ex) 
    {
    	ACS_SHORT_LOG ((LM_INFO, "Ok, exception received while building a non existent node"));
    }
	return 0;
}

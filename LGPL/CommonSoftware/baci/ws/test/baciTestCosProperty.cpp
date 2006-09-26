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
* "@(#) $Id: baciTestCosProperty.cpp,v 1.3 2006/09/26 06:26:32 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2005-09-27 created
*/
 
static char *rcsId="@(#) $Id: baciTestCosProperty.cpp,v 1.3 2006/09/26 06:26:32 cparedes Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <tao/corba.h>
#include <ace/Read_Buffer.h>

#include <string>

#include <baci.h>
#include <baciCORBA.h>
#include <baciC.h>
#include <baciS.h>
#include <baciTestC.h>
#include <logging.h>
#include <baciTest.h>
#include <baciTestUtils.h>

 using namespace baci;
 using namespace BACI_TEST;

unsigned int sleep(unsigned int);

#ifdef MAKE_VXWORKS
int startBaciTestClient (int argc,  char **argv)
#else
int main (int argc, char **argv)
#endif
{
    

    try
	{
	// create logging proxy
	LoggingProxy *m_logger = new LoggingProxy(0, 0, 31, 0);
	LoggingProxy::init(m_logger);
	LoggingProxy::ProcessName(argv[0]);
	LoggingProxy::ThreadName("main");
	ACS_TEST_INIT_LOGGING;
	
	// 
	// Initialysation of CORBA, POA and related CORBA internals  
	// 
	ACE_CString g_strCmdLn;
	for (int i=argc-1; i>=0; i--)
	    g_strCmdLn = ACE_CString(argv[i])+ " " + g_strCmdLn;

	if (g_strCmdLn.find("-ORBDottedDecimalAddresses")==ACE_CString::npos)
	    g_strCmdLn += " -ORBDottedDecimalAddresses 1";

	ACE_TCHAR **m_argv = argv;
	int m_argc = argc;
	ACE_OS::string_to_argv((ACE_TCHAR*)g_strCmdLn.c_str(),
			       m_argc,
			       m_argv);
	
	BACI_CORBA::InitCORBA(m_argc, m_argv);
	
	
	std::string readIOR;
	
	int result = read_IOR_from_file ("BACI1", readIOR);
	if (result != 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR, "Cannot read IOR from file"));
	    return -1;
	    }
	
	// Get an object reference from the argument string.
	CORBA::Object_var object = 
	    BACI_CORBA::getORB()->string_to_object (readIOR.c_str());
	
	
	if (CORBA::is_nil(object.in())) 
	    {
	    ACS_SHORT_LOG ((LM_DEBUG, "Cannot create OBJ from IOR"));
	    return -1;
	    }
	
	// Try to narrow the object reference to a PS reference.
	BACI_TEST::BaciTestClass_var ps = BACI_TEST::BaciTestClass::_narrow(object.in());
	
	CORBA::String_var ior = BACI_CORBA::getORB()->object_to_string (ps.in());
									
	ACS_SHORT_LOG((LM_INFO,"baciTestClient: Connecting to: %s", ior.in()));
									
	//---------------------------------------------------------------

	CosPropertyService::PropertySet_var propertySet = ps->get_all_characteristics();

	try
	    {
	    CORBA::Any anyval;
	    propertySet->define_property("new", anyval);
	    
	    ACS_SHORT_LOG((LM_INFO,"baciTestClient: CosPropertyService::ReadOnlyProperty was not thrown!"));
	    }
	catch (CosPropertyService::ReadOnlyProperty&)
	    {
	    // OK
	    }

	CORBA::ULong count = propertySet->get_number_of_properties ();
	ACS_SHORT_LOG((LM_INFO,"baciTestClient: get_number_of_properties = %d", count));
  
	CORBA::Any_var val = propertySet->get_property_value("");
	const char* strVal;
	val >>= CORBA::Any::to_string(strVal, 0);
	ACS_SHORT_LOG((LM_INFO,"baciTestClient: get_property_value(\"\") = %s", strVal));

	CosPropertyService::PropertyNames_var names;
	CosPropertyService::PropertyNamesIterator_var rest;
	propertySet->get_all_property_names (100, names.out(), rest.out());

	ACS_SHORT_LOG((LM_INFO,"baciTestClient: get_all_property_names() = "));
	for (CORBA::ULong n = 0; n < names->length(); n++)
	    ACS_SHORT_LOG((LM_INFO,"\t%s", names[n].in()));


	try
	    {
	    propertySet->delete_property("ROdoubleProp");
	    
	    ACS_SHORT_LOG((LM_INFO,"baciTestClient: CosPropertyService::FixedProperty was not thrown!"));
	    }
	catch (CosPropertyService::FixedProperty&)
	    {
	    // OK
	    }

	CORBA::Boolean isDefined = propertySet->is_property_defined("ROdoubleProp");
	ACS_SHORT_LOG((LM_INFO,"baciTestClient: is_property_defined(\"ROdoubleProp\") = %d", isDefined));

	isDefined = propertySet->is_property_defined("invalid");
	ACS_SHORT_LOG((LM_INFO,"baciTestClient: is_property_defined(\"invalid\") = %d", isDefined));


	{
	ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to get ROdoubleProp..."));
	ACS::ROdouble_var prop = ps->ROdoubleProp();
	
	propertySet = prop->get_all_characteristics();

	CORBA::ULong count = propertySet->get_number_of_properties ();
	ACS_SHORT_LOG((LM_INFO,"baciTestClient: get_number_of_properties = %d", count));
  
	CORBA::Any_var val = propertySet->get_property_value("");
	const char* strVal;
	val >>= CORBA::Any::to_string(strVal, 0);
	ACS_SHORT_LOG((LM_INFO,"baciTestClient: get_property_value(\"\") = %s", strVal));

	CosPropertyService::PropertyNames_var names;
	CosPropertyService::PropertyNamesIterator_var rest;
	propertySet->get_all_property_names (100, names.out(), rest.out());

	ACS_SHORT_LOG((LM_INFO,"baciTestClient: get_all_property_names() = "));
	for (CORBA::ULong n = 0; n < names->length(); n++)
	    ACS_SHORT_LOG((LM_INFO,"\t%s", names[n].in()));
	}


	//---------------------------------------------------------------

	ACS_SHORT_LOG((LM_INFO,"baciTestClient: main thread, shutting down..."));


	ps->shutdown();
	
	sleep(10);

	BACI_CORBA::DoneCORBA();
	
	// Delete the logger last.
	delete m_logger;

	}
    catch(CORBA::Exception &ex)
	{
        ACE_PRINT_EXCEPTION (ACE_ANY_EXCEPTION,"Error!");
        return -1;
	}
    ACE_CHECK_RETURN (-1);

    // Wait for the servant to complete cleanup before exiting.
    sleep(2);
    return 0;

} /* end main() */






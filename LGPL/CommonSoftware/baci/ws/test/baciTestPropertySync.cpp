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
* "@(#) $Id: baciTestPropertySync.cpp,v 1.102 2006/10/16 07:56:40 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2002-01-03 Added ROstring, RWpattern
* gchiozzi 2001-12-19 Added initialisation of standard LoggingProxy fields
*/
 
static char *rcsId="@(#) $Id: baciTestPropertySync.cpp,v 1.102 2006/10/16 07:56:40 cparedes Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <tao/corba.h>
#include <ace/Read_Buffer.h>

#include <string>
#include <sstream>

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

#ifdef MAKE_VXWORKS
unsigned int sleep(unsigned int);
#endif

template <class T_property>
void testRO( T_property myProp ) {

   ACSErr::Completion_var completion;
   ACS_SHORT_LOG((LM_INFO,"baciTestClient: .. name is: %s", myProp->name()));

   std::ostringstream oss;
   oss << "baciTestClient: .. value is: " << myProp->get_sync(completion.out()) ;
   ACS_SHORT_LOG((LM_INFO, oss.str().c_str()));
}

template <class T_value, class T_property> 
void testRW( T_property myProp, T_value setValue ) {

   ACSErr::Completion_var completion;
   T_value orig_value;
   T_value new_value;

   // Getting the RWdoubleProp value
   orig_value = myProp->get_sync(completion.out());
	
   std::ostringstream oss;
   oss << "baciTestClient: .. value is: " << orig_value << std::ends;
   ACS_SHORT_LOG((LM_INFO, oss.str().c_str()));

   // Setting a new value and read it back
   new_value = setValue;
   completion = myProp->set_sync(new_value);

   oss.str("");
   oss << "baciTestClient: .. set value to: " << new_value << std::ends;
   ACS_SHORT_LOG((LM_INFO, oss.str().c_str()));
	
   new_value = myProp->get_sync(completion.out());
	
   oss.str("");
   oss << "baciTestClient: .. value is: " << new_value << std::ends;
   ACS_SHORT_LOG((LM_INFO, oss.str().c_str()));

   // Reset to original value
   completion = myProp->set_sync(orig_value);
	
   oss.str("");
   oss << "baciTestClient: .. reset value to: " << orig_value << std::ends;
   ACS_SHORT_LOG((LM_INFO, oss.str().c_str()));
	
   new_value = myProp->get_sync(completion.out());

   oss.str("");
   oss << "baciTestClient: .. value is: " << new_value << std::ends;
   ACS_SHORT_LOG((LM_INFO, oss.str().c_str()));
}
	
template<class T_value, class T_valueSeq, class T_property>
void testRWSeq( T_property myProp, T_valueSeq new_value_ds, T_value new_value ) {

   ACSErr::Completion_var completion;
   T_valueSeq orig_value_ds;

   // Getting the RWdoubleSeqProp value
   orig_value_ds = myProp->get_sync(completion.out());
	
   if (orig_value_ds.ptr() == 0)
   {
     ACS_SHORT_LOG((LM_INFO,"baciTestClient: .. null value returned."));
   }
   else
   {
     ACS_SHORT_LOG((LM_INFO,"baciTestClient: .. value is (length %u):", orig_value_ds->length()));
      for (CORBA::ULong i = 0; i < orig_value_ds->length(); i++)
      {
        ACS_SHORT_LOG((LM_INFO,"\t(%u): %f", i, orig_value_ds[i]));
      }
   }

   if (new_value_ds.ptr() != 0) {
     new_value_ds->length(128);
     for (CORBA::ULong i = 0; i < new_value_ds->length(); i++)
	new_value_ds[i] = new_value;
   }

   completion = myProp->set_sync(new_value_ds.in());
	
   if (new_value_ds.ptr() == 0)
   {
      ACS_SHORT_LOG((LM_INFO,"baciTestClient: .. set value to: null value."));
   }
   else
   {
     ACS_SHORT_LOG((LM_INFO,"baciTestClient: .. set value to (length %u):", new_value_ds->length()));
     for (CORBA::ULong i = 0; i < new_value_ds->length(); i++)
     {
       std::ostringstream oss;
       oss << "\t(" << i << "): " << new_value_ds[i];
       ACS_SHORT_LOG((LM_INFO,oss.str().c_str()));
     }
   }

   new_value_ds = myProp->get_sync(completion.out());

   if (new_value_ds.ptr() == 0)
   {
      ACS_SHORT_LOG((LM_INFO,"baciTestClient: .. null value returned."));
   }
   else
   {
     ACS_SHORT_LOG((LM_INFO,"baciTestClient: .. value is (length %u):", new_value_ds->length()));
     for (CORBA::ULong i = 0; i < new_value_ds->length(); i++)
     {
       std::ostringstream oss;
       oss << "\t(" << i << "): " << new_value_ds[i];
       ACS_SHORT_LOG((LM_INFO,oss.str().c_str()));
     }
   }

   // Reset to original value
   completion = myProp->set_sync(orig_value_ds.in());

   if (orig_value_ds.ptr() == 0)
   {
      ACS_SHORT_LOG((LM_INFO,"baciTestClient: .. reset value to: null value."));
   }
   else
   {
     ACS_SHORT_LOG((LM_INFO,"baciTestClient: .. reset value to (length %u):", orig_value_ds->length()));
     for (CORBA::ULong i = 0; i < orig_value_ds->length(); i++)
     {
       std::ostringstream oss;
       oss << "\t(" << i << "): " << orig_value_ds[i];
       ACS_SHORT_LOG((LM_INFO,oss.str().c_str()));
     }
   }

   new_value_ds = myProp->get_sync(completion.out());

   if (new_value_ds.ptr() == 0)
   {
     ACS_SHORT_LOG((LM_INFO,"baciTestClient: .. null value returned."));
   }
   else
   {
     ACS_SHORT_LOG((LM_INFO,"baciTestClient: .. value is (length %u):", new_value_ds->length()));
     for (CORBA::ULong i = 0; i < new_value_ds->length(); i++)
     {
       std::ostringstream oss;
       oss << "\t(" << i << "): " << new_value_ds[i];
       ACS_SHORT_LOG((LM_INFO,oss.str().c_str()));
     }
   }
}

template <class T_valueSeq, class T_property>
void testROSeq( T_property myProp ) {

   ACSErr::Completion_var completion;
   T_valueSeq orig_value_ds = myProp->get_sync(completion.out());

   if (orig_value_ds.ptr() == 0)
   {
     ACS_SHORT_LOG((LM_INFO,"baciTestClient: .. null value returned."));
   }
   else
   { 
     ACS_SHORT_LOG((LM_INFO,"baciTestClient: .. value is (length %u):", orig_value_ds->length()));
     for (CORBA::ULong i = 0; i < orig_value_ds->length(); i++)
     {
       std::ostringstream oss;
       oss << "\t(" << i << "): " << orig_value_ds[i];
       ACS_SHORT_LOG((LM_INFO,oss.str().c_str()));
     }
   }
}

//-------------------------------------------------------------

int main (int argc, char **argv)
{
    
    
    if(argc !=2)
	{
	printf("Give a number as argument\n");
	exit(1);
	}
    
    try{
	ACSErr::Completion_var completion;

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
	

	if (CORBA::is_nil(object.in())) {
	  ACS_SHORT_LOG ((LM_DEBUG, "Cannot create OBJ from IOR"));
	  return -1;
	}

	// Try to narrow the object reference to a 
        // BACI_TEST::BaciTestClass reference.
	BACI_TEST::BaciTestClass_var ps = BACI_TEST::BaciTestClass::_narrow (object.in ());
	

	CORBA::String_var ior =
	  BACI_CORBA::getORB()->object_to_string (ps.in ()
						  );
	
      
        ACS_SHORT_LOG((LM_INFO,"baciTestClient: Connecting to: %s", ior.in ()));

	//--------------------------------------------------------------
        // testing property

        ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to get ACS::RWdouble RWdoubleWithDevIOProp..."));
        testRW<CORBA::Double,ACS::RWdouble_var>(ps->RWdoubleWithDevIOProp(), atof(argv[1]));
        ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to get ACS::RWdouble RWdoubleProp..."));
        testRW<CORBA::Double,ACS::RWdouble_var>(ps->RWdoubleProp(), atof(argv[1]));

        ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to get ACS::ROdouble ROdoubleProp..."));
        testRO<ACS::ROdouble_var>(ps->ROdoubleProp());

        ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to get ACS::RWfloat RWfloatProp..."));
        testRW<CORBA::Float,ACS::RWfloat_var>(ps->RWfloatProp(), atof(argv[1]));

        ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to get ACS::ROfloat ROfloatProp..."));
        testRO<ACS::ROfloat_var>(ps->ROfloatProp());

        ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to get ACS::RWlong RWlongProp..."));
        testRW<CORBA::Long,ACS::RWlong_var>(ps->RWlongProp(), atol(argv[1]));

        ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to get ACS::ROlong ROlongProp..."));
        testRO<ACS::ROlong_var>(ps->ROlongProp());

        ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to get ACS::RWpattern RWpatternProp..."));
        testRW<ACS::pattern,ACS::RWpattern_var>(ps->RWpatternProp(), atoi(argv[1]));

        ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to get ACS::ROpattern ROpatternProp..."));
        testRO<ACS::ROpattern_var>(ps->ROpatternProp());

        ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to get ACS::RWstring RWstringProp..."));
        testRW<CORBA::String_var,ACS::RWstring_var>(ps->RWstringProp(), "New string");

        ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to get ACS::ROstring ROstringProp..."));
        testRO<ACS::ROstring_var>(ps->ROstringProp());

	ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to get ACS::RWdoubleSeq RWdoubleSeqProp..."));
        ACS::doubleSeq_var new_seq = new ACS::doubleSeq();
	testRWSeq<CORBA::Double, ACS::doubleSeq_var, ACS::RWdoubleSeq_var>(ps->RWdoubleSeqProp(),new_seq,atof(argv[1]));

	ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to get ACS::ROdoubleSeq ROdoubleSeqProp..."));
	testROSeq<ACS::doubleSeq_var, ACS::ROdoubleSeq_var>(ps->ROdoubleSeqProp());
        
	ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to get ACS::RWfloatSeq RWfloatSeqProp..."));
        ACS::floatSeq_var new_seq_f = new ACS::floatSeq();
	testRWSeq<CORBA::Float, ACS::floatSeq_var, ACS::RWfloatSeq_var>(ps->RWfloatSeqProp(),new_seq_f,atof(argv[1]));

	ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to get ACS::ROfloatSeq ROfloatSeqProp..."));
	testROSeq<ACS::floatSeq_var, ACS::ROfloatSeq_var>(ps->ROfloatSeqProp());
        
	ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to get ACS::RWlongSeq RWlongSeqProp..."));
        ACS::longSeq_var new_seq_l = new ACS::longSeq();
	testRWSeq<CORBA::Long, ACS::longSeq_var, ACS::RWlongSeq_var>(ps->RWlongSeqProp(),new_seq_l,atol(argv[1]));

	ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to get ACS::ROlongSeq ROlongSeqProp..."));
	testROSeq<ACS::longSeq_var, ACS::ROlongSeq_var>(ps->ROlongSeqProp());
        
	//---------------------------------------------------------------
	// The End
	//---------------------------------------------------------------
	// ACS_SHORT_LOG((LM_INFO,"baciTestClient: main thread..."));
	ACS_SHORT_LOG((LM_INFO,"baciTestClient: .... The End."));

	ps->shutdown();
	
	sleep(10);

	BACI_CORBA::DoneCORBA();
	delete m_logger;
    }
  catch(CORBA::Exception &ex)
    {
	ACS_SHORT_LOG((LM_INFO,"Exception: .... The End."));
        ACE_PRINT_EXCEPTION (ACE_ANY_EXCEPTION,"Error!");
        return -1;
    }
  ACE_CHECK_RETURN (-1);

  return 0;

} /* end main() */






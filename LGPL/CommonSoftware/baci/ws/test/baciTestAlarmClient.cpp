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
 * "@(#) $Id: baciTestAlarmClient.cpp,v 1.4 2009/09/25 13:59:26 bjeram Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * oat      2008-02-02 created
 */
 
static char *rcsId="@(#) $Id: baciTestAlarmClient.cpp,v 1.4 2009/09/25 13:59:26 bjeram Exp $";
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

#include <acsThread.h>

#include <ace/SString.h>

using namespace ACS;
using namespace baci;
using namespace BACI_TEST;


/**
 * Used to provide common functionality for the implementations of BACI 
 * callbacks and alarms.  Really all this class does is keep track of the number
 * of done invocations as well as the name of the BACI property it's monitoring, receiving
 * an asynchronous value from, etc.
 * 
 * @version "@(#) $Id: baciTestAlarmClient.cpp,v 1.4 2009/09/25 13:59:26 bjeram Exp $"
 */
class CommonCallback
{
  public:
    /**
     * Standard constructor
     */
    CommonCallback() {}
    
  protected:
    /**
     * 99% of the time, callback are used in conjunction with BACI properties.
     * This member is just the name of that property.
     */
    ACE_CString prop;
    
    /**
     * To keep the acsexmpl modular test deterministic, we keep track of the number
     * of times the done method is invoked.  From this value the logging priority
     * is dynamically changed under some circumstances.
     */
    unsigned int m_count;

  private:
    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const CommonCallback&);
};


/////////////////////////////////////////////////////////////////////////
/**
 * This class is the implementation of the AlarmPattern IDL interface defined in baci.idl.
 * It is used so that we may create a local alarm for the value of a remote BACI 
 * property.  There are only two useful methods: alarm_raised and alarm_cleared.  They
 * do just what their names imply.
 * 
 * @version "@(#) $Id: baciTestAlarmClient.cpp,v 1.4 2009/09/25 13:59:26 bjeram Exp $"
 */
class MyAlarmpattern : public virtual POA_ACS::Alarmpattern,    //CORBA servant stub
		       protected CommonCallback
{
  public:
    /**
     * Constructor
     * @param _prop Name of this Alarmdouble instance
     */
    MyAlarmpattern(ACE_CString _prop)
	{
	    prop = _prop;
	}
    
    /**
     * Destructor - nothing to delete.
     */
    ~MyAlarmpattern() {}

    /**
     * Method invoked when the double value goes out of range.
     * @param value The double's current (i.e., out of range) value
     * @param c Error handing structure.
     * @param desc Callback descriptor
     * @return void
     * @htmlonly
     <br><hr>
     @endhtmlonly
    */
    void 
    alarm_raised (ACS::pattern value,
		  const ACSErr::Completion &c,
		  const ACS::CBDescOut &desc)
	{
	    ACS_SHORT_LOG ((LM_INFO, "(%s::Alarmpattern::alarm_raised) Value: %d", prop.c_str(), value));
	}

	
    /**
     * Method invoked when the double value goes back into the acceptable range.
     * @param value The double's new value
     * @param c Error handing structure.
     * @param desc Callback descriptor
     * @return void
     * @htmlonly
     <br><hr>
     @endhtmlonly
    */
    void 
    alarm_cleared (ACS::pattern value,
		   const ACSErr::Completion &c,
		   const ACS::CBDescOut &desc)
	{
	    ACS_SHORT_LOG ((LM_INFO, "(%s::Alarmpattern::alarm_cleared) Value: %d", prop.c_str(), value));
	}

	    
    /**
     * Method used so that client and servant can agree
     * upon the time it takes to transmit data (generally invocations
     * of the done method). A smart callback implementation would analize
     * the value of time_to_transmit and decide whether the value is acceptable,
     * but we just return true regardless in this simplistic example.
     * @param time_to_transmit Time to transmit data.
     * @param desc Callback descriptor
     * @return True regardless of parameter values.
     * @htmlonly
     <br><hr>
     @endhtmlonly
    */
    CORBA::Boolean 
    negotiate (ACS::TimeInterval time_to_transmit, const ACS::CBDescOut &desc) 
	{
	    return true;
	}

};



class WorkerThreadPattern : public ACS::Thread
{ 
  public:
    WorkerThreadPattern(const ACE_CString& name,
    		BACI_TEST::BaciTestAlarmClass* comp,
			const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
			const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime) :
	ACS::Thread(name, responseTime, sleepTime)
	{
	    ACS_TRACE("WorkerThreadPattern::WorkerThreadPattern");
	    comp_m = BACI_TEST::BaciTestAlarmClass::_duplicate(comp);
	    rwPattern_m = comp_m->rwPatternProperty();
	    count = 0;
	    numValues = 9;
	    values = new int[numValues];
	                    // 1011 - alarm_mask read from the CDB (decimal 11), three bits considered
                            // 1101 - alarm_trigger read from the CDB (decimal 13)
                            // 0010 - default value read from the CDB (decimal 2), no alarm raised 
	    values[0] = 10; // 0110 - 1 bit present in alarm_trigger, 1 alarm raised
	    values[1] = 2;  // 0010 - no bit present in alarm_trigger, alarm cleared
	    values[2] = 1;  // 0001 - 2 bits present in alarm_trigger, 2 alarms raised
	    values[3] = 3;  // 0011 - 1 bit present in alarm_trigger, 1 alarm cleared
	    values[4] = 2;  // 0010 - no bit present in alarm_trigger, the other alarm cleared 
	    values[5] = 9;  // 1001 - 3 bits present in alarm_trigger, 3 alarms raised
	    values[6] = 2;  // 0010 - no bit present in alarm_trigger, all 3 alarms cleared
	    values[7] = 6;  // 0110 - the bit present in alarm_trigger is not present in alarm_mask, no alarm raised
	    //we added this that we have an alarm before we change FF and FM
	    values[8] = 1;  // 0001 - 2 bits present in alarm_trigger, 2 alarms raised

	    ACS_SHORT_LOG((LM_INFO, "%s: Created thread", getName().c_str()));
	}

    ~WorkerThreadPattern() 
	{ 
	    ACS_TRACE("WorkerThreadPattern::~WorkerThreadPattern"); 
	    if(NULL != values) 
		{
		delete[] values;
		}
	}

    virtual void runLoop()
	{
	    if(0 == count) {
	    ACS_SHORT_LOG((LM_INFO, "%s: Started runLoop for thread", getName().c_str()));
	    }
	    //while there are still values that need to be set...
	    if(count < numValues)
		{
		try
		    {


		    //change the BACI property's value synchronously
		    ACS_SHORT_LOG((LM_INFO, "%s: Setting rwPattern to %d", getName().c_str(), values[count]));
		    rwPattern_m->set_sync(values[count]);
		    count++;

		    ACSErr::Completion_var completion;
		    prop_m = rwPattern_m->get_sync(completion.out());
		    cout << "rwPatternProperty: " << prop_m << endl;

		    ACE_OS::sleep(1);

		    }
		catch(...)
		    {
		    ACS_SHORT_LOG((LM_ERROR,"Error!"));
		    }
		}
	    else {
	    	ACS_SHORT_LOG((LM_INFO, "==> Going to test changing of FF and FM if we have a previous alarm."));
	    	// first heaving an alarm
	    	comp_m->changeAlarmFFFM("UserDefinedFF", "UserDefinedFM");

	    	// reset all alarms
	    	ACS_SHORT_LOG((LM_INFO, "==> Going to test changing of FF and FM if we do not have a previous alarm."));
	    	ACS_SHORT_LOG((LM_INFO, "==> First we reset all alarms an wait that are actaully cleared."));
	    	ACS_SHORT_LOG((LM_INFO, "%s: Setting rwPattern to %d", getName().c_str(), 2));
			rwPattern_m->set_sync(2);
			ACE_OS::sleep(2); //we have to wait that alarm is actually cleaned
			ACS_SHORT_LOG((LM_INFO, "==> After resting alarms  we set new FF FM"));
			comp_m->changeAlarmFFFM("AnotherUserDefinedFF", "AnotherUserDefinedFM");

			ACS_SHORT_LOG((LM_INFO, "==>  Generate an alarm after we have changed FF, FM."));
	    	ACS_SHORT_LOG((LM_INFO, "%s: Setting rwPattern to %d", getName().c_str(), 1));
			rwPattern_m->set_sync(1);

	    	setStopped();
	    	ACS_SHORT_LOG((LM_INFO, "%s: Stopped thread", getName().c_str()));
	    }
	}

  private:
    ACS::RWpattern_var rwPattern_m;
    int count;
    int numValues;
    //Ranges in the CDB for roPattern are:
    // alarm low  on : 5
    // alarm high on : 80
    int * values; 
    ACS::pattern prop_m;
    BACI_TEST::BaciTestAlarmClass_var comp_m;
};


unsigned int sleep(unsigned int);


#ifdef MAKE_VXWORKS
////////////////////////////int startBaciTestClient (int argc,  char **argv)
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

	int result = read_IOR_from_file ("BACIALARM", readIOR);
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

	// Try to narrow the object reference to a BaciTestAlarmClass reference.
	BACI_TEST::BaciTestAlarmClass_var comp = BACI_TEST::BaciTestAlarmClass::_narrow(object.in());
	

	CORBA::String_var ior =
	    BACI_CORBA::getORB()->object_to_string (comp.in());
	
      
        ACS_SHORT_LOG((LM_INFO,"baciTestAlarmClient: Connecting to: %s", ior.in()));

	/********************* pattern alarm subscription ******************/


	ACS::ROpattern_var roPattern = comp->roPatternProperty();
	ACS::RWpattern_var rwPattern = comp->rwPatternProperty();

	//create an instance of our alarm class
	MyAlarmpattern macb("roPatternProperty");
	//activate it as a CORBA object
	ACS::Alarmpattern_var acb = macb._this(); 
	//create the actual BACI double alarm
	ACS::CBDescIn desc;
	ACS::Subscription_var alarmSub = roPattern->new_subscription_Alarm(acb.in(), desc);
	ACS_SHORT_LOG((LM_INFO,"Alarmpattern subscription created"));
	

	// create the thread 
	WorkerThreadPattern threadPattern_p ("actionThreadPattern", comp, ThreadBase::defaultResponseTime, ThreadBase::defaultSleepTime*10 /*=1s*/);
	// by default threads that are not created using a thread manager are creatd suspended so we have to resume them!!
	threadPattern_p.resume();
	
	//---------------------------------------------------------------

	//Enter main loop and stays there for a fixed amount of time.  Really we are
	//just allowing the thread we just created to run for awhile before exiting out
	//of this example.
	ACS_SHORT_LOG((LM_INFO,"baciTestAlarmClient: main thread, entering ORB loop to sleep..."));

	ACE_Time_Value tv(25);

	BACI_CORBA::getORB()->run(tv); 


	//Must cleanly destroy the alarm
	ACS_SHORT_LOG((LM_INFO,"Alarm subscriptions deleted")); 
	alarmSub->destroy();	


	ACS_SHORT_LOG((LM_INFO,"baciTestAlarmClient: main thread, shutting down..."));


	comp->shutdown();
	
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






#ifndef ALARM_TEST_SERVER_IMPL_H
#define ALARM_TEST_SERVER_IMPL_H

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "AlarmTestServerS.h"
#include <acsutilTimeStamp.h>
#include <baciCharacteristicComponentImpl.h>
#include <baciROdouble.h>

namespace jbaci { namespace test {

	class HardwareSimulator : public ACS::Thread
	{
	public:
		HardwareSimulator(const ACE_CString &name,
		                  const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime,
		                  const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime)
	     : ACS::Thread(name, responseTime, sleepTime)
		{
			startTime = getTimeStamp();
			timestamp = getTimeStamp();
			frequency = 1.0;             // 1 Hz
			dVal = 0.0;
			ACS_TRACE("HardwareSimulator::HardwareSimulator()");
		}

		~HardwareSimulator()
		{
			ACS_TRACE("HardwareSimulator::~HardwareSimulator()");
		}

		virtual void onStart();

		virtual void runLoop();

		double getDouble(ACS::Time &ts);

	private:
		static const double pi = 3.1415926535897;

		ACS::Time startTime;  // in the unit of 100 ns.
		ACS::Time timestamp;  // in the unit of 100 ns.
		double frequency;     // Hz
		CORBA::Double dVal;	};//class HardwareSimulator

	template <class T> class HardwareSimulatorDevIO : public DevIO<T>
	{
	public:
		HardwareSimulatorDevIO(HardwareSimulator *hwSim_p,
		                       T (HardwareSimulator::*get_fp)(ACS::Time &ts)) :
			hwSim_mp(hwSim_p),
			get_mfp(get_fp)
			{};

		virtual ~HardwareSimulatorDevIO() {};

		virtual bool initializationValue() { return false; }

		virtual T read(ACS::Time& timestamp)
		{
			if (hwSim_mp) {

				return (hwSim_mp->*get_mfp)(timestamp);
			} else {
				return 0;
			}
		}
	private:
		HardwareSimulator *hwSim_mp;
		T (HardwareSimulator::*get_mfp)(ACS::Time &ts);
	};

	class AlarmTestServerImpl :
			public baci::CharacteristicComponentImpl,
			public POA_jbaci::AlarmTestServer
	{
	public:
		AlarmTestServerImpl(const ACE_CString& name,
		                    maci::ContainerServices * containerServices);

		~AlarmTestServerImpl();

		void execute();

		// componnet's life cycle
		void cleanUp();

		// implementations of IDL's methods
		ACS::ROdouble_ptr doubleProp ();

	private:
	 
		HardwareSimulator *m_hwSim_p;

		baci::ROdouble *m_doubleProp_p;
		HardwareSimulatorDevIO<double> *m_doubleDevIO;

	    /**
	     * ALMA C++ coding standards state copy operators should be disabled.
	     */
		void operator=(const AlarmTestServerImpl&);

	};//class AlarmTestServerImpl

};//namespace test
};//namespace jbaci


#endif /*!_H*/

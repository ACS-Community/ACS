#include "vltPort.h"

static char *rcsId="@(#) $Id$";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "AlarmTestServerImpl.h"
#include <cmath>

using namespace jbaci::test;
using namespace baci;

void HardwareSimulator::onStart()
{
	startTime = getTimeStamp();
}

void HardwareSimulator::runLoop()
{
	ACS::TimeInterval t;

	// TODO Acquire mutex to atomically update timestamp and the values.
	timestamp = getTimeStamp();

	t = timestamp - startTime;
	dVal = std::sin(2.0 * pi * frequency * ((double)t / 10000000.0));

	ACS_TRACE("HardwareSimulator::runLoop()");
}

double HardwareSimulator::getDouble(ACS::Time &ts)
{
	double ret;

	// TODO Acquire mutex before obtaining the value and the timestamp.
	//      because they are updated in a separate thread.
	ts = timestamp;
	ret = dVal;
	ACS_TRACE("HardwareSimulator::getDouble()");

	return ret;
}


AlarmTestServerImpl::AlarmTestServerImpl(const ACE_CString& name,
			     maci::ContainerServices * containerServices)
    : baci::CharacteristicComponentImpl(name, containerServices),
    m_doubleProp_p(0)
{
    AUTO_TRACE("AlarmTestServerImpl::AlarmTestServerImpl");

    ACS::TimeInterval loopInterval = 100000; // Update the internal values every 10 ms (100,000 * 100 ns).
    m_hwSim_p = getContainerServices()->getThreadManager()->create<HardwareSimulator>("hwSimLoop", ACS::ThreadBase::defaultResponseTime, loopInterval);

    m_doubleDevIO = new HardwareSimulatorDevIO<double>(m_hwSim_p, &HardwareSimulator::getDouble);
    m_doubleProp_p  = new ROdouble(name+":doubleProp", getComponent(), m_doubleDevIO);
    CHARACTERISTIC_COMPONENT_PROPERTY(doubleProp, m_doubleProp_p);

}//AlarmTestServerImpl

AlarmTestServerImpl::~AlarmTestServerImpl()
{
    AUTO_TRACE("AlarmTestServerImpl::~AlarmTestServerImpl");

    getContainerServices()->getThreadManager()->destroy(m_hwSim_p);

    if (m_doubleProp_p != 0)
    {
    	m_doubleProp_p->destroy();
    	m_doubleProp_p=0;
    }
    delete m_doubleDevIO;

}//~AlarmTestServerImpl


void AlarmTestServerImpl::execute()
{
  if (m_hwSim_p) {
    m_hwSim_p->resume();
  }
}

void AlarmTestServerImpl::cleanUp()
{
  if (m_hwSim_p) {
    m_hwSim_p->stop();
  }
}//cleanUp

ACS::ROdouble_ptr AlarmTestServerImpl::doubleProp ()
{
	if (m_doubleProp_p == 0)
	{
		return ACS::ROdouble::_nil();
	}

	ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_doubleProp_p ->getCORBAReference());
	return prop._retn();
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(AlarmTestServerImpl)
/* ----------------------------------------------------------------*/


/*___oOo___*/

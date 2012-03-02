/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2009
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: MCtestAlarmsComponentImpl.cpp,v 1.1 2012/03/02 14:03:10 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2009-02-11  created
*/

#include "vltPort.h"

static char *rcsId="@(#) $Id: MCtestAlarmsComponentImpl.cpp,v 1.1 2012/03/02 14:03:10 tstaig Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "MCtestAlarmsComponentImpl.h"


using namespace TMCDB;
using namespace baci;



MCtestAlarmsComponentImpl::MCtestAlarmsComponentImpl(const ACE_CString& name, maci::ContainerServices * containerServices):
	baci::CharacteristicComponentImpl(name, containerServices),
	m_doubleROProp_p(0),
	m_floatROProp_p(0),
	m_longROProp_p(0),
	m_longLongROProp_p(0),
	m_uLongLongROProp_p(0),
	m_doubleSeqROProp_p(0),
	m_floatSeqROProp_p(0),
	m_longSeqROProp_p(0)
{
	AUTO_TRACE("MCtestAlarmsComponentImpl::MCtestAlarmsComponentImpl");
	//Initialize Values
	m_doubleROVal = 1.0;
	m_time1 = 134608945243381570;
	m_doubleRODevIO = new MCtestDevIONoIncremental<CORBA::Double>(m_doubleROVal, m_time1);
	m_doubleROProp_p = new ROdouble(name+":doubleROProp", getComponent(), m_doubleRODevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(doubleROProp, m_doubleROProp_p);
	m_floatROVal = 1.0;
	m_time2 = 134608945243381570;
	m_floatRODevIO = new MCtestDevIONoIncremental<CORBA::Float>(m_floatROVal, m_time2);
	m_floatROProp_p = new ROfloat(name+":floatROProp", getComponent(), m_floatRODevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(floatROProp, m_floatROProp_p);
	m_longROVal = 1;
	m_time3 = 134608945243381570;
	m_longRODevIO = new MCtestDevIONoIncremental<CORBA::Long>(m_longROVal, m_time3);
	m_longROProp_p = new ROlong(name+":longROProp", getComponent(), m_longRODevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(longROProp, m_longROProp_p);
	m_longLongROVal = 1;
	m_time5 = 134608945243381570;
	m_longLongRODevIO = new MCtestDevIONoIncremental<ACS::longLong>(m_longLongROVal, m_time5);
	m_longLongROProp_p = new ROlongLong(name+":longLongROProp", getComponent(), m_longLongRODevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(longLongROProp, m_longLongROProp_p);
	m_uLongLongROVal = 1;
	m_time6 = 134608945243381570;
	m_uLongLongRODevIO = new MCtestDevIONoIncremental<ACS::uLongLong>(m_uLongLongROVal, m_time6);
	m_uLongLongROProp_p = new ROuLongLong(name+":uLongLongROProp", getComponent(), m_uLongLongRODevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(uLongLongROProp, m_uLongLongROProp_p);
	m_doubleSeqROVal.length(2);
	for(unsigned int i=0;i<m_doubleSeqROVal.length();i++)
		m_doubleSeqROVal[i]=1.0;
	m_time7 = 134608945243381570;
	m_doubleSeqRODevIO = new MCtestDevIOSeqNoIncremental<ACS::doubleSeq>(m_doubleSeqROVal, m_time7);
	m_doubleSeqROProp_p = new ROdoubleSeq(name+":doubleSeqROProp", getComponent(), m_doubleSeqRODevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(doubleSeqROProp, m_doubleSeqROProp_p);
	m_floatSeqROVal.length(2);
	for(unsigned int i=0;i<m_floatSeqROVal.length();i++)
		m_floatSeqROVal[i]=1.0;
	m_time8 = 134608945243381570;
	m_floatSeqRODevIO = new MCtestDevIOSeqNoIncremental<ACS::floatSeq>(m_floatSeqROVal, m_time8);
	m_floatSeqROProp_p = new ROfloatSeq(name+":floatSeqROProp", getComponent(), m_floatSeqRODevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(floatSeqROProp, m_floatSeqROProp_p);
	m_longSeqROVal.length(2);
	for(unsigned int i=0;i<m_longSeqROVal.length();i++)
		m_longSeqROVal[i]=1;
	m_time9 = 134608945243381570;
	m_longSeqRODevIO = new MCtestDevIOSeqNoIncremental<ACS::longSeq>(m_longSeqROVal, m_time9);
	m_longSeqROProp_p = new ROlongSeq(name+":longSeqROProp", getComponent(), m_longSeqRODevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(longSeqROProp, m_longSeqROProp_p);

}//MCtestAlarmsComponentImpl

MCtestAlarmsComponentImpl::~MCtestAlarmsComponentImpl()
{
	AUTO_TRACE("MCtestAlarmsComponentImpl::~MCtestAlarmsComponentImpl");
	if(m_doubleROProp_p != 0)
	{
		m_doubleROProp_p->destroy();
		m_doubleROProp_p=0;
	}
	delete m_doubleRODevIO;
	if(m_floatROProp_p != 0)
	{
		m_floatROProp_p->destroy();
		m_floatROProp_p=0;
	}
	delete m_floatRODevIO;
	if(m_longROProp_p != 0)
	{
		m_longROProp_p->destroy();
		m_longROProp_p=0;
	}
	delete m_longRODevIO;
	if(m_longLongROProp_p != 0)
	{
		m_longLongROProp_p->destroy();
		m_longLongROProp_p=0;
	}
	delete m_longLongRODevIO;
	if(m_uLongLongROProp_p != 0)
	{
		m_uLongLongROProp_p->destroy();
		m_uLongLongROProp_p=0;
	}
	delete m_uLongLongRODevIO;
	if(m_doubleSeqROProp_p != 0)
	{
		m_doubleSeqROProp_p->destroy();
		m_doubleSeqROProp_p=0;
	}
	delete m_doubleSeqRODevIO;
	if(m_floatSeqROProp_p != 0)
	{
		m_floatSeqROProp_p->destroy();
		m_floatSeqROProp_p=0;
	}
	delete m_floatSeqRODevIO;
	if(m_longSeqROProp_p != 0)
	{
		m_longSeqROProp_p->destroy();
		m_longSeqROProp_p=0;
	}
	delete m_longSeqRODevIO;
}//~MCtestAlarmsComponentImpl


void MCtestAlarmsComponentImpl::execute()
{

}

void MCtestAlarmsComponentImpl::cleanUp()
{

}//cleanUp

ACS::ROdouble_ptr MCtestAlarmsComponentImpl::doubleROProp()
{
	if(m_doubleROProp_p == 0)
	{
		return ACS::ROdouble::_nil();
	}

	ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_doubleROProp_p->getCORBAReference());
	return prop._retn();
}

ACS::ROfloat_ptr MCtestAlarmsComponentImpl::floatROProp()
{
	if(m_floatROProp_p == 0)
	{
		return ACS::ROfloat::_nil();
	}

	ACS::ROfloat_var prop = ACS::ROfloat::_narrow(m_floatROProp_p->getCORBAReference());
	return prop._retn();
}

ACS::ROlong_ptr MCtestAlarmsComponentImpl::longROProp()
{
	if(m_longROProp_p == 0)
	{
		return ACS::ROlong::_nil();
	}

	ACS::ROlong_var prop = ACS::ROlong::_narrow(m_longROProp_p->getCORBAReference());
	return prop._retn();
}

ACS::ROlongLong_ptr MCtestAlarmsComponentImpl::longLongROProp()
{
	if(m_longLongROProp_p == 0)
	{
		return ACS::ROlongLong::_nil();
	}

	ACS::ROlongLong_var prop = ACS::ROlongLong::_narrow(m_longLongROProp_p->getCORBAReference());
	return prop._retn();
}

ACS::ROuLongLong_ptr MCtestAlarmsComponentImpl::uLongLongROProp()
{
	if(m_uLongLongROProp_p == 0)
	{
		return ACS::ROuLongLong::_nil();
	}

	ACS::ROuLongLong_var prop = ACS::ROuLongLong::_narrow(m_uLongLongROProp_p->getCORBAReference());
	return prop._retn();
}

ACS::ROdoubleSeq_ptr MCtestAlarmsComponentImpl::doubleSeqROProp()
{
	if(m_doubleSeqROProp_p == 0)
	{
		return ACS::ROdoubleSeq::_nil();
	}

	ACS::ROdoubleSeq_var prop = ACS::ROdoubleSeq::_narrow(m_doubleSeqROProp_p->getCORBAReference());
	return prop._retn();
}

ACS::ROfloatSeq_ptr MCtestAlarmsComponentImpl::floatSeqROProp()
{
	if(m_floatSeqROProp_p == 0)
	{
		return ACS::ROfloatSeq::_nil();
	}

	ACS::ROfloatSeq_var prop = ACS::ROfloatSeq::_narrow(m_floatSeqROProp_p->getCORBAReference());
	return prop._retn();
}

ACS::ROlongSeq_ptr MCtestAlarmsComponentImpl::longSeqROProp()
{
	if(m_longSeqROProp_p == 0)
	{
		return ACS::ROlongSeq::_nil();
	}

	ACS::ROlongSeq_var prop = ACS::ROlongSeq::_narrow(m_longSeqROProp_p->getCORBAReference());
	return prop._retn();
}

void MCtestAlarmsComponentImpl::reset()
{
	m_doubleROProp_p->getDevIO()->write(m_doubleROVal, m_time1);
	m_floatROProp_p->getDevIO()->write(m_floatROVal, m_time2);
	m_longROProp_p->getDevIO()->write(m_longROVal, m_time3);
	m_longLongROProp_p->getDevIO()->write(m_longLongROVal, m_time5);
	m_uLongLongROProp_p->getDevIO()->write(m_uLongLongROVal, m_time6);
	m_doubleSeqROProp_p->getDevIO()->write(m_doubleSeqROVal, m_time7);
	m_floatSeqROProp_p->getDevIO()->write(m_floatSeqROVal, m_time8);
	m_longSeqROProp_p->getDevIO()->write(m_longSeqROVal, m_time9);
}

void MCtestAlarmsComponentImpl::increase()
{
	m_doubleROProp_p->getDevIO()->write(++m_doubleROVal, m_time1);
	m_floatROProp_p->getDevIO()->write(++m_floatROVal, m_time2);
	m_longROProp_p->getDevIO()->write(++m_longROVal, m_time3);
	m_longLongROProp_p->getDevIO()->write(++m_longLongROVal, m_time5);
	m_uLongLongROProp_p->getDevIO()->write(++m_uLongLongROVal, m_time6);
	for(unsigned int i=0;i<m_doubleSeqROVal.length();i++)
		m_doubleSeqROVal[i]++;
	m_doubleSeqROProp_p->getDevIO()->write(m_doubleSeqROVal, m_time7);
	for(unsigned int i=0;i<m_floatSeqROVal.length();i++)
		m_floatSeqROVal[i]++;
	m_floatSeqROProp_p->getDevIO()->write(m_floatSeqROVal, m_time8);
	for(unsigned int i=0;i<m_longSeqROVal.length();i++)
		m_longSeqROVal[i]++;
	m_longSeqROProp_p->getDevIO()->write(m_longSeqROVal, m_time9);
}

void MCtestAlarmsComponentImpl::decrease()
{
	m_doubleROProp_p->getDevIO()->write(--m_doubleROVal, m_time1);
	m_floatROProp_p->getDevIO()->write(--m_floatROVal, m_time2);
	m_longROProp_p->getDevIO()->write(--m_longROVal, m_time3);
	m_longLongROProp_p->getDevIO()->write(--m_longLongROVal, m_time5);
	m_uLongLongROProp_p->getDevIO()->write(--m_uLongLongROVal, m_time6);
	for(unsigned int i=0;i<m_doubleSeqROVal.length();i++)
		m_doubleSeqROVal[i]--;
	m_doubleSeqROProp_p->getDevIO()->write(m_doubleSeqROVal, m_time7);
	for(unsigned int i=0;i<m_floatSeqROVal.length();i++)
		m_floatSeqROVal[i]--;
	m_floatSeqROProp_p->getDevIO()->write(m_floatSeqROVal, m_time8);
	for(unsigned int i=0;i<m_longSeqROVal.length();i++)
		m_longSeqROVal[i]--;
	m_longSeqROProp_p->getDevIO()->write(m_longSeqROVal, m_time9);
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(MCtestAlarmsComponentImpl)
/* ----------------------------------------------------------------*/


/*___oOo___*/

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
* "@(#) $Id: MCtestPropertiesComponentImpl.cpp,v 1.1 2011/05/23 19:31:44 javarias Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* tstaig  2011-05-02  created
*/

#include "vltPort.h"

static char *rcsId="@(#) $Id: MCtestPropertiesComponentImpl.cpp,v 1.1 2011/05/23 19:31:44 javarias Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "MCtestPropertiesComponentImpl.h"

using namespace TMCDB;
using namespace baci;

MCtestPropertiesComponentImpl::MCtestPropertiesComponentImpl(const ACE_CString& name,
				maci::ContainerServices * containerServices):
	baci::CharacteristicComponentImpl(name, containerServices),
	m_doubleROProp_p(0),
	m_floatROProp_p(0),
	m_longROProp_p(0),
	m_patternROProp_p(0),
	m_stringROProp_p(0),
	m_longLongROProp_p(0),
	m_uLongLongROProp_p(0),
	m_doubleSeqROProp_p(0),
	m_floatSeqROProp_p(0),
	m_longSeqROProp_p(0),
	m_doubleRWProp_p(0),
	m_floatRWProp_p(0),
	m_longRWProp_p(0),
	m_patternRWProp_p(0),
	m_stringRWProp_p(0),
	m_longLongRWProp_p(0),
	m_uLongLongRWProp_p(0),
	m_doubleSeqRWProp_p(0),
	m_floatSeqRWProp_p(0),
	m_longSeqRWProp_p(0)
{
	AUTO_TRACE("MCtestPropertiesComponentImpl::MCtestPropertiesComponentImpl");
	//Initialize Values
	m_doubleROVal = 0.0;
	m_time1 = 134608945243381570;
	m_doubleRODevIO = new MCtestDevIONoIncremental<CORBA::Double>(m_doubleROVal, m_time1);
	m_doubleROProp_p = new ROdouble(name+":doubleROProp", getComponent(), m_doubleRODevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(doubleROProp, m_doubleROProp_p);
	m_floatROVal = 0.0;
	m_time2 = 134608945243381570;
	m_floatRODevIO = new MCtestDevIONoIncremental<CORBA::Float>(m_floatROVal, m_time2);
	m_floatROProp_p = new ROfloat(name+":floatROProp", getComponent(), m_floatRODevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(floatROProp, m_floatROProp_p);
	m_longROVal = 0;
	m_time3 = 134608945243381570;
	m_longRODevIO = new MCtestDevIONoIncremental<CORBA::Long>(m_longROVal, m_time3);
	m_longROProp_p = new ROlong(name+":longROProp", getComponent(), m_longRODevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(longROProp, m_longROProp_p);
	m_patternROVal = 0;
	m_time5 = 134608945243381570;
	m_patternRODevIO = new MCtestDevIONoIncremental<ACS::pattern>(m_patternROVal, m_time5);
	m_patternROProp_p = new ROpattern(name+":patternROProp", getComponent(), m_patternRODevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(patternROProp, m_patternROProp_p);
	m_time6 = 134608945243381570;
	m_stringRODevIO = new MCtestDevIONoIncremental<ACE_CString>(m_stringROVal, m_time6);
	m_stringROProp_p = new ROstring(name+":stringROProp", getComponent(), m_stringRODevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(stringROProp, m_stringROProp_p);
	m_longLongROVal = 0;
	m_time7 = 134608945243381570;
	m_longLongRODevIO = new MCtestDevIONoIncremental<ACS::longLong>(m_longLongROVal, m_time7);
	m_longLongROProp_p = new ROlongLong(name+":longLongROProp", getComponent(), m_longLongRODevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(longLongROProp, m_longLongROProp_p);
	m_uLongLongROVal = 0;
	m_time8 = 134608945243381570;
	m_uLongLongRODevIO = new MCtestDevIONoIncremental<ACS::uLongLong>(m_uLongLongROVal, m_time8);
	m_uLongLongROProp_p = new ROuLongLong(name+":uLongLongROProp", getComponent(), m_uLongLongRODevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(uLongLongROProp, m_uLongLongROProp_p);
   m_doubleSeqROVal.length(2);
	for(unsigned int i=0;i<m_doubleSeqROVal.length();i++)
		m_doubleSeqROVal[i]=0.0;
	m_time9 = 134608945243381570;
	m_doubleSeqRODevIO = new MCtestDevIOSeqNoIncremental<ACS::doubleSeq>(m_doubleSeqROVal, m_time9);
	m_doubleSeqROProp_p = new ROdoubleSeq(name+":doubleSeqROProp", getComponent(), m_doubleSeqRODevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(doubleSeqROProp, m_doubleSeqROProp_p);
   m_floatSeqROVal.length(2);
	for(unsigned int i=0;i<m_floatSeqROVal.length();i++)
		m_floatSeqROVal[i]=0.0;
	m_time10 = 134608945243381570;
	m_floatSeqRODevIO = new MCtestDevIOSeqNoIncremental<ACS::floatSeq>(m_floatSeqROVal, m_time10);
	m_floatSeqROProp_p = new ROfloatSeq(name+":floatSeqROProp", getComponent(), m_floatSeqRODevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(floatSeqROProp, m_floatSeqROProp_p);
   m_longSeqROVal.length(2);
	for(unsigned int i=0;i<m_longSeqROVal.length();i++)
		m_longSeqROVal[i]=0;
	m_time11 = 134608945243381570;
	m_longSeqRODevIO = new MCtestDevIOSeqNoIncremental<ACS::longSeq>(m_longSeqROVal, m_time11);
	m_longSeqROProp_p = new ROlongSeq(name+":longSeqROProp", getComponent(), m_longSeqRODevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(longSeqROProp, m_longSeqROProp_p);
	m_doubleRWVal = 0;
	m_time17 = 134608945243381570;
	m_doubleRWDevIO = new MCtestDevIONoIncremental<CORBA::Double>(m_doubleRWVal, m_time17);
	m_doubleRWProp_p = new RWdouble(name+":doubleRWProp", getComponent(), m_doubleRWDevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(doubleRWProp, m_doubleRWProp_p);
	m_floatRWVal = 0;
	m_time18 = 134608945243381570;
	m_floatRWDevIO = new MCtestDevIONoIncremental<CORBA::Float>(m_floatRWVal, m_time18);
	m_floatRWProp_p = new RWfloat(name+":floatRWProp", getComponent(), m_floatRWDevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(floatRWProp, m_floatRWProp_p);
	m_longRWVal = 0;
	m_time19 = 134608945243381570;
	m_longRWDevIO = new MCtestDevIONoIncremental<CORBA::Long>(m_longRWVal, m_time19);
	m_longRWProp_p = new RWlong(name+":longRWProp", getComponent(), m_longRWDevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(longRWProp, m_longRWProp_p);
	m_patternRWVal = 0;
	m_time21 = 134608945243381570;
	m_patternRWDevIO = new MCtestDevIONoIncremental<ACS::pattern>(m_patternRWVal, m_time21);
	m_patternRWProp_p = new RWpattern(name+":patternRWProp", getComponent(), m_patternRWDevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(patternRWProp, m_patternRWProp_p);
	m_time22 = 134608945243381570;
	m_stringRWDevIO = new MCtestDevIONoIncremental<ACE_CString>(m_stringRWVal, m_time22);
	m_stringRWProp_p = new RWstring(name+":stringRWProp", getComponent(), m_stringRWDevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(stringRWProp, m_stringRWProp_p);
	m_longLongRWVal = 0;
	m_time23 = 134608945243381570;
	m_longLongRWDevIO = new MCtestDevIONoIncremental<ACS::longLong>(m_longLongRWVal, m_time23);
	m_longLongRWProp_p = new RWlongLong(name+":longLongRWProp", getComponent(), m_longLongRWDevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(longLongRWProp, m_longLongRWProp_p);
	m_uLongLongRWVal = 0;
	m_time24 = 134608945243381570;
	m_uLongLongRWDevIO = new MCtestDevIONoIncremental<ACS::uLongLong>(m_uLongLongRWVal, m_time24);
	m_uLongLongRWProp_p = new RWuLongLong(name+":uLongLongRWProp", getComponent(), m_uLongLongRWDevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(uLongLongRWProp, m_uLongLongRWProp_p);
   m_doubleSeqRWVal.length(2);
	for(unsigned int i=0;i<m_doubleSeqRWVal.length();i++)
		m_doubleSeqRWVal[i]=0.0;
	m_time25 = 134608945243381570;
	m_doubleSeqRWDevIO = new MCtestDevIOSeqNoIncremental<ACS::doubleSeq>(m_doubleSeqRWVal, m_time25);
	m_doubleSeqRWProp_p = new RWdoubleSeq(name+":doubleSeqRWProp", getComponent(), m_doubleSeqRWDevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(doubleSeqRWProp, m_doubleSeqRWProp_p);
   m_floatSeqRWVal.length(2);
	for(unsigned int i=0;i<m_floatSeqRWVal.length();i++)
		m_floatSeqRWVal[i]=0.0;
	m_time26 = 134608945243381570;
	m_floatSeqRWDevIO = new MCtestDevIOSeqNoIncremental<ACS::floatSeq>(m_floatSeqRWVal, m_time26);
	m_floatSeqRWProp_p = new RWfloatSeq(name+":floatSeqRWProp", getComponent(), m_floatSeqRWDevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(floatSeqRWProp, m_floatSeqRWProp_p);
   m_longSeqRWVal.length(2);
	for(unsigned int i=0;i<m_longSeqRWVal.length();i++)
		m_longSeqRWVal[i]=0;
	m_time27 = 134608945243381570;
	m_longSeqRWDevIO = new MCtestDevIOSeqNoIncremental<ACS::longSeq>(m_longSeqRWVal, m_time27);
	m_longSeqRWProp_p = new RWlongSeq(name+":longSeqRWProp", getComponent(), m_longSeqRWDevIO);
	CHARACTERISTIC_COMPONENT_PROPERTY(longSeqRWProp, m_longSeqRWProp_p);
}//MCtestPropertiesComponentImpl

MCtestPropertiesComponentImpl::~MCtestPropertiesComponentImpl()
{
    AUTO_TRACE("MCtestPropertiesComponentImpl::~MCtestPropertiesComponentImpl");
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
	if(m_patternROProp_p != 0)
	{
		m_patternROProp_p->destroy();
		m_patternROProp_p=0;
	}
	delete m_patternRODevIO;
	if(m_stringROProp_p != 0)
	{
		m_stringROProp_p->destroy();
		m_stringROProp_p=0;
	}
	//delete m_stringRODevIO;
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
	if(m_doubleRWProp_p != 0)
	{
		m_doubleRWProp_p->destroy();
		m_doubleRWProp_p=0;
	}
	delete m_doubleRWDevIO;
	if(m_floatRWProp_p != 0)
	{
		m_floatRWProp_p->destroy();
		m_floatRWProp_p=0;
	}
	delete m_floatRWDevIO;
	if(m_longRWProp_p != 0)
	{
		m_longRWProp_p->destroy();
		m_longRWProp_p=0;
	}
	delete m_longRWDevIO;
	if(m_patternRWProp_p != 0)
	{
		m_patternRWProp_p->destroy();
		m_patternRWProp_p=0;
	}
	delete m_patternRWDevIO;
	if(m_stringRWProp_p != 0)
	{
		m_stringRWProp_p->destroy();
		m_stringRWProp_p=0;
	}
	//delete m_stringRWDevIO;
	if(m_longLongRWProp_p != 0)
	{
		m_longLongRWProp_p->destroy();
		m_longLongRWProp_p=0;
	}
	delete m_longLongRWDevIO;
	if(m_uLongLongRWProp_p != 0)
	{
		m_uLongLongRWProp_p->destroy();
		m_uLongLongRWProp_p=0;
	}
	delete m_uLongLongRWDevIO;
	if(m_doubleSeqRWProp_p != 0)
	{
		m_doubleSeqRWProp_p->destroy();
		m_doubleSeqRWProp_p=0;
	}
	delete m_doubleSeqRWDevIO;
	if(m_floatSeqRWProp_p != 0)
	{
		m_floatSeqRWProp_p->destroy();
		m_floatSeqRWProp_p=0;
	}
	delete m_floatSeqRWDevIO;
	if(m_longSeqRWProp_p != 0)
	{
		m_longSeqRWProp_p->destroy();
		m_longSeqRWProp_p=0;
	}
	delete m_longSeqRWDevIO;
}//~MCtestPropertiesComponentImpl


void MCtestPropertiesComponentImpl::execute()
{

}

void MCtestPropertiesComponentImpl::cleanUp()
{

}//cleanUp

ACS::ROdouble_ptr MCtestPropertiesComponentImpl::doubleROProp()
{
	if(m_doubleROProp_p == 0)
	{
		return ACS::ROdouble::_nil();
	}

	ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_doubleROProp_p->getCORBAReference());
	return prop._retn();
}
ACS::ROfloat_ptr MCtestPropertiesComponentImpl::floatROProp()
{
	if(m_floatROProp_p == 0)
	{
		return ACS::ROfloat::_nil();
	}

	ACS::ROfloat_var prop = ACS::ROfloat::_narrow(m_floatROProp_p->getCORBAReference());
	return prop._retn();
}
ACS::ROlong_ptr MCtestPropertiesComponentImpl::longROProp()
{
	if(m_longROProp_p == 0)
	{
		return ACS::ROlong::_nil();
	}

	ACS::ROlong_var prop = ACS::ROlong::_narrow(m_longROProp_p->getCORBAReference());
	return prop._retn();
}
ACS::ROpattern_ptr MCtestPropertiesComponentImpl::patternROProp()
{
	if(m_patternROProp_p == 0)
	{
		return ACS::ROpattern::_nil();
	}

	ACS::ROpattern_var prop = ACS::ROpattern::_narrow(m_patternROProp_p->getCORBAReference());
	return prop._retn();
}
ACS::ROstring_ptr MCtestPropertiesComponentImpl::stringROProp()
{
	if(m_stringROProp_p == 0)
	{
		return ACS::ROstring::_nil();
	}

	ACS::ROstring_var prop = ACS::ROstring::_narrow(m_stringROProp_p->getCORBAReference());
	return prop._retn();
}
ACS::ROlongLong_ptr MCtestPropertiesComponentImpl::longLongROProp()
{
	if(m_longLongROProp_p == 0)
	{
		return ACS::ROlongLong::_nil();
	}

	ACS::ROlongLong_var prop = ACS::ROlongLong::_narrow(m_longLongROProp_p->getCORBAReference());
	return prop._retn();
}
ACS::ROuLongLong_ptr MCtestPropertiesComponentImpl::uLongLongROProp()
{
	if(m_uLongLongROProp_p == 0)
	{
		return ACS::ROuLongLong::_nil();
	}

	ACS::ROuLongLong_var prop = ACS::ROuLongLong::_narrow(m_uLongLongROProp_p->getCORBAReference());
	return prop._retn();
}
ACS::ROdoubleSeq_ptr MCtestPropertiesComponentImpl::doubleSeqROProp()
{
	if(m_doubleSeqROProp_p == 0)
	{
		return ACS::ROdoubleSeq::_nil();
	}

	ACS::ROdoubleSeq_var prop = ACS::ROdoubleSeq::_narrow(m_doubleSeqROProp_p->getCORBAReference());
	return prop._retn();
}
ACS::ROfloatSeq_ptr MCtestPropertiesComponentImpl::floatSeqROProp()
{
	if(m_floatSeqROProp_p == 0)
	{
		return ACS::ROfloatSeq::_nil();
	}

	ACS::ROfloatSeq_var prop = ACS::ROfloatSeq::_narrow(m_floatSeqROProp_p->getCORBAReference());
	return prop._retn();
}
ACS::ROlongSeq_ptr MCtestPropertiesComponentImpl::longSeqROProp()
{
	if(m_longSeqROProp_p == 0)
	{
		return ACS::ROlongSeq::_nil();
	}

	ACS::ROlongSeq_var prop = ACS::ROlongSeq::_narrow(m_longSeqROProp_p->getCORBAReference());
	return prop._retn();
}
ACS::RWdouble_ptr MCtestPropertiesComponentImpl::doubleRWProp()
{
	if(m_doubleRWProp_p == 0)
	{
		return ACS::RWdouble::_nil();
	}

	ACS::RWdouble_var prop = ACS::RWdouble::_narrow(m_doubleRWProp_p->getCORBAReference());
	return prop._retn();
}
ACS::RWfloat_ptr MCtestPropertiesComponentImpl::floatRWProp()
{
	if(m_floatRWProp_p == 0)
	{
		return ACS::RWfloat::_nil();
	}

	ACS::RWfloat_var prop = ACS::RWfloat::_narrow(m_floatRWProp_p->getCORBAReference());
	return prop._retn();
}
ACS::RWlong_ptr MCtestPropertiesComponentImpl::longRWProp()
{
	if(m_longRWProp_p == 0)
	{
		return ACS::RWlong::_nil();
	}

	ACS::RWlong_var prop = ACS::RWlong::_narrow(m_longRWProp_p->getCORBAReference());
	return prop._retn();
}
ACS::RWpattern_ptr MCtestPropertiesComponentImpl::patternRWProp()
{
	if(m_patternRWProp_p == 0)
	{
		return ACS::RWpattern::_nil();
	}

	ACS::RWpattern_var prop = ACS::RWpattern::_narrow(m_patternRWProp_p->getCORBAReference());
	return prop._retn();
}
ACS::RWstring_ptr MCtestPropertiesComponentImpl::stringRWProp()
{
	if(m_stringRWProp_p == 0)
	{
		return ACS::RWstring::_nil();
	}

	ACS::RWstring_var prop = ACS::RWstring::_narrow(m_stringRWProp_p->getCORBAReference());
	return prop._retn();
}
ACS::RWlongLong_ptr MCtestPropertiesComponentImpl::longLongRWProp()
{
	if(m_longLongRWProp_p == 0)
	{
		return ACS::RWlongLong::_nil();
	}

	ACS::RWlongLong_var prop = ACS::RWlongLong::_narrow(m_longLongRWProp_p->getCORBAReference());
	return prop._retn();
}
ACS::RWuLongLong_ptr MCtestPropertiesComponentImpl::uLongLongRWProp()
{
	if(m_uLongLongRWProp_p == 0)
	{
		return ACS::RWuLongLong::_nil();
	}

	ACS::RWuLongLong_var prop = ACS::RWuLongLong::_narrow(m_uLongLongRWProp_p->getCORBAReference());
	return prop._retn();
}
ACS::RWdoubleSeq_ptr MCtestPropertiesComponentImpl::doubleSeqRWProp()
{
	if(m_doubleSeqRWProp_p == 0)
	{
		return ACS::RWdoubleSeq::_nil();
	}

	ACS::RWdoubleSeq_var prop = ACS::RWdoubleSeq::_narrow(m_doubleSeqRWProp_p->getCORBAReference());
	return prop._retn();
}
ACS::RWfloatSeq_ptr MCtestPropertiesComponentImpl::floatSeqRWProp()
{
	if(m_floatSeqRWProp_p == 0)
	{
		return ACS::RWfloatSeq::_nil();
	}

	ACS::RWfloatSeq_var prop = ACS::RWfloatSeq::_narrow(m_floatSeqRWProp_p->getCORBAReference());
	return prop._retn();
}
ACS::RWlongSeq_ptr MCtestPropertiesComponentImpl::longSeqRWProp()
{
	if(m_longSeqRWProp_p == 0)
	{
		return ACS::RWlongSeq::_nil();
	}

	ACS::RWlongSeq_var prop = ACS::RWlongSeq::_narrow(m_longSeqRWProp_p->getCORBAReference());
	return prop._retn();
}

void MCtestPropertiesComponentImpl::reset()
{
	//m_doubleProp_p->getDevIO()->write(m_doubleVal, m_time2);

	//m_doubleSeqProp_p->getDevIO()->write(m_doubleSeqVal, m_time1);
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(MCtestPropertiesComponentImpl)
/* ----------------------------------------------------------------*/


/*___oOo___*/

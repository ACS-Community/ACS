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
* "@(#) $Id: MCtestComponentImpl.cpp,v 1.2 2011/02/07 16:13:58 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2009-02-11  created
*/

#include "vltPort.h"

static char *rcsId="@(#) $Id: MCtestComponentImpl.cpp,v 1.2 2011/02/07 16:13:58 tstaig Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "MCtestComponentImpl.h"


using namespace TMCDB;
using namespace baci;



MCtestComponentImpl::MCtestComponentImpl(const ACE_CString& name,
			     maci::ContainerServices * containerServices)
    : baci::CharacteristicComponentImpl(name, containerServices),
    m_doubleProp_p(0),
    m_doubleSeqProp_p(0),
    m_longSeqProp_p(0),
    m_longProp_p(0)
{
    AUTO_TRACE("MCtestComponentImpl::MCtestComponentImpl");

    m_doubleSeqVal.length(25);
    //Changed starting value to -1.0 since the value is read for initialization
    //purposes in the monitoring loop.
    for(unsigned int i=0;i<m_doubleSeqVal.length();i++)
    	m_doubleSeqVal[i]=-2.0;
    m_time1 = 134608945243381570;
    m_doubleSeqDevIO = new MCtestDevIOSeq<ACS::doubleSeq>(m_doubleSeqVal, m_time1);
    m_doubleSeqProp_p  = new ROdoubleSeq(name+":doubleSeqProp", getComponent(), m_doubleSeqDevIO);
    CHARACTERISTIC_COMPONENT_PROPERTY(doubleSeqProp, m_doubleSeqProp_p);

    //Changed starting value to -1.0 since the value is read for initialization
    //purposes in the monitoring loop.
    m_doubleVal = -2.0;
    m_time2 = 134608945243381570;
    m_doubleDevIO = new MCtestDevIO<double>(m_doubleVal, m_time2);
    m_doubleProp_p  = new ROdouble(name+":doubleProp", getComponent(), m_doubleDevIO);
    CHARACTERISTIC_COMPONENT_PROPERTY(doubleProp, m_doubleProp_p);

    m_longVal = 15;
    m_time3 = 134608945243381570;
    m_longDevIO = new MCtestDevIO<int>(m_longVal, m_time3);
    m_longProp_p  = new RWlong(name+":longProp", getComponent(), m_longDevIO);
    CHARACTERISTIC_COMPONENT_PROPERTY(longProp, m_longProp_p);

    m_longSeqVal.length(25);
    for(unsigned int i=0;i<m_longSeqVal.length();i++)
    	m_longSeqVal[i]=-2;
    m_time4 = 134608945243381570;
    m_longSeqDevIO = new MCtestDevIOSeq<ACS::longSeq>(m_longSeqVal, m_time4);
    m_longSeqProp_p  = new RWlongSeq(name+":longSeqProp", getComponent(), m_longSeqDevIO);
    CHARACTERISTIC_COMPONENT_PROPERTY(longSeqProp, m_longSeqProp_p);

    m_patternVal = 0x23; //00100011
    m_time5 = 134608945243381570;
    m_patternDevIO = new MCtestDevIO<ACS::pattern>(m_patternVal, m_time5);
    m_patternProp_p  = new ROpattern(name+":patternProp", getComponent(), m_patternDevIO);
    CHARACTERISTIC_COMPONENT_PROPERTY(patternProp, m_patternProp_p);
}//MCtestComponentImpl

MCtestComponentImpl::~MCtestComponentImpl()
{
    AUTO_TRACE("MCtestComponentImpl::~MCtestComponentImpl");
    if (m_doubleSeqProp_p != 0)
    {
    	m_doubleSeqProp_p->destroy();
    	m_doubleSeqProp_p=0;
    }
    if (m_doubleProp_p != 0)
    {
    	m_doubleProp_p->destroy();
    	m_doubleProp_p=0;
    }
    delete m_doubleDevIO;


    if (m_longSeqProp_p != 0)
    {
    	m_longSeqProp_p->destroy();
    	m_longSeqProp_p=0;
    }
    if (m_longProp_p != 0)
    {
    	m_longProp_p->destroy();
    	m_longProp_p=0;
    }
    delete m_longDevIO;
    if (m_patternProp_p != 0)
    {
    	m_patternProp_p->destroy();
    	m_patternProp_p=0;
    }
    delete m_patternDevIO;

}//~MCtestComponentImpl


void MCtestComponentImpl::execute()
{

}

void MCtestComponentImpl::cleanUp()
{

}//cleanUp


ACS::ROdoubleSeq_ptr MCtestComponentImpl::doubleSeqProp ()
{
	if (m_doubleSeqProp_p == 0)
	{
		return ACS::ROdoubleSeq::_nil();
	}

	ACS::ROdoubleSeq_var prop = ACS::ROdoubleSeq::_narrow(m_doubleSeqProp_p ->getCORBAReference());
	return prop._retn();
}

ACS::ROdouble_ptr MCtestComponentImpl::doubleProp ()
{
	if (m_doubleProp_p == 0)
	{
		return ACS::ROdouble::_nil();
	}

	ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_doubleProp_p ->getCORBAReference());
	return prop._retn();
}

ACS::RWlongSeq_ptr MCtestComponentImpl::longSeqProp ()
{
	if (m_longSeqProp_p == 0)
	{
		return ACS::RWlongSeq::_nil();
	}

	ACS::RWlongSeq_var prop = ACS::RWlongSeq::_narrow(m_longSeqProp_p ->getCORBAReference());
	return prop._retn();
}

ACS::RWlong_ptr MCtestComponentImpl::longProp ()
{
	if (m_longProp_p == 0)
	{
		return ACS::RWlong::_nil();
	}

	ACS::RWlong_var prop = ACS::RWlong::_narrow(m_longProp_p ->getCORBAReference());
	return prop._retn();
}

ACS::ROpattern_ptr MCtestComponentImpl::patternProp ()
{
	if (m_patternProp_p == 0)
	{
		return ACS::ROpattern::_nil();
	}

	ACS::ROpattern_var prop = ACS::ROpattern::_narrow(m_patternProp_p ->getCORBAReference());
	return prop._retn();
}

void MCtestComponentImpl::reset()
{
	m_doubleProp_p->getDevIO()->write(m_doubleVal, m_time2);

	m_doubleSeqProp_p->getDevIO()->write(m_doubleSeqVal, m_time1);
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(MCtestComponentImpl)
/* ----------------------------------------------------------------*/


/*___oOo___*/

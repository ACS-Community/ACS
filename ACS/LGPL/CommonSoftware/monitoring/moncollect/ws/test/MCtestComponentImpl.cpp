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
    m_doubleSeqProp_p(0)
{
    AUTO_TRACE("MCtestComponentImpl::MCtestComponentImpl");

    m_doubleSeqVal.length(25);
    //Changed starting value to -1.0 since the value is read for initialization
    //purposes in the monitoring loop.
    for(unsigned int i=0;i<m_doubleSeqVal.length();i++)
    	m_doubleSeqVal[i]=-1.0;
    m_time1 = 134608945243381570;
    m_doubleSeqDevIO = new MCtestDevIOSeq<ACS::doubleSeq>(m_doubleSeqVal, m_time1);
    m_doubleSeqProp_p  = new ROdoubleSeq(name+":doubleSeqProp", getComponent(), m_doubleSeqDevIO);
    CHARACTERISTIC_COMPONENT_PROPERTY(doubleSeqProp, m_doubleSeqProp_p);

    //Changed starting value to -1.0 since the value is read for initialization
    //purposes in the monitoring loop.
    m_doubleVal = -1.0;
    m_time2 = 134608945243381570;
    m_doubleDevIO = new MCtestDevIO<double>(m_doubleVal, m_time2);
    m_doubleProp_p  = new ROdouble(name+":doubleProp", getComponent(), m_doubleDevIO);
    CHARACTERISTIC_COMPONENT_PROPERTY(doubleProp, m_doubleProp_p);

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

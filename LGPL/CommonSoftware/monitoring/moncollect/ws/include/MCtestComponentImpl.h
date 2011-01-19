#ifndef MC_TESTCOMPONENT_IMPL_H
#define MC_TESTCOMPONENT_IMPL_H
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
* "@(#) $Id: MCtestComponentImpl.h,v 1.1 2011/01/19 21:20:41 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2009-02-11  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "MCtestComponentS.h"
#include "MCtestDevIO.h"
#include <baciCharacteristicComponentImpl.h>
#include <baciROdoubleSeq.h>
#include <baciROdouble.h>

namespace TMCDB
{

/**
 * Monitor collector implementation
 */
class MCtestComponentImpl :
	public baci::CharacteristicComponentImpl,
	public POA_TMCDB::MCtestComponent
{
public:
	MCtestComponentImpl(const ACE_CString& name,
			maci::ContainerServices * containerServices);

	~MCtestComponentImpl();

	void execute();

	// componnet's life cycle
	void cleanUp();


	// implementations of IDL's methods
	 ACS::ROdoubleSeq_ptr doubleSeqProp ();

	 ACS::ROdouble_ptr doubleProp ();

	 void reset();

private:


	    baci::ROdouble *m_doubleProp_p;
	    ACS::doubleSeq m_doubleSeqVal;
	    ACS::Time m_time1;
	    MCtestDevIOSeq<ACS::doubleSeq> *m_doubleSeqDevIO;

	    baci::ROdoubleSeq *m_doubleSeqProp_p;
	    double m_doubleVal;
	    ACS::Time m_time2;
	    MCtestDevIO<double> *m_doubleDevIO;


	    /**
	     * ALMA C++ coding standards state copy operators should be disabled.
	     */
	    void operator=(const MCtestComponentImpl&);

};//class MCtestComponentImpl



};//namespace TMCDB


#endif /*!_H*/

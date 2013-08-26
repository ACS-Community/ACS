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
* "@(#) $Id: MCtestAlarmsComponentImpl.h,v 1.2 2012/10/10 09:48:53 bjeram Exp $"
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
#include <baciROdouble.h>
#include <baciROfloat.h>
#include <baciROlong.h>
#include <baciROuLong.h>
#include <baciROlongLong.h>
#include <baciROuLongLong.h>
#include <baciROboolean.h>
#include <baciROdoubleSeq.h>
#include <baciROfloatSeq.h>
#include <baciROlongSeq.h>
#include <baciROuLongSeq.h>
#include <baciRObooleanSeq.h>
#include <enumpropROImpl.h>

namespace TMCDB
{
	/**
	 * Monitor collector implementation
	 */
	class MCtestAlarmsComponentImpl :
		public baci::CharacteristicComponentImpl,
		public POA_TMCDB::MCtestAlarmsComponent
	{
	  public:
		MCtestAlarmsComponentImpl(const ACE_CString& name, maci::ContainerServices * containerServices);
		~MCtestAlarmsComponentImpl();

		// componnet's life cycle
		void execute();
		void cleanUp();
	
		// implementations of IDL's methods
		ACS::ROdouble_ptr doubleROProp();
		ACS::ROfloat_ptr floatROProp();
		ACS::ROlong_ptr longROProp();
		ACS::ROuLong_ptr uLongROProp();
		ACS::ROlongLong_ptr longLongROProp();
		ACS::ROuLongLong_ptr uLongLongROProp();
		ACS::ROboolean_ptr booleanROProp();
		ACS::ROdoubleSeq_ptr doubleSeqROProp();
		ACS::ROfloatSeq_ptr floatSeqROProp();
		ACS::ROlongSeq_ptr longSeqROProp();
		ACS::ROuLongSeq_ptr uLongSeqROProp();
		ACS::RObooleanSeq_ptr booleanSeqROProp();
		ROEnumTest_ptr EnumTestROProp();
		void reset();
		void increase();
		void decrease();

	  private:
		baci::ROdouble *m_doubleROProp_p;
		CORBA::Double m_doubleROVal;
		ACS::Time m_time1;
		DevIO<CORBA::Double> *m_doubleRODevIO;
		baci::ROfloat *m_floatROProp_p;
		CORBA::Float m_floatROVal;
		ACS::Time m_time2;
		DevIO<CORBA::Float> *m_floatRODevIO;
		baci::ROlong *m_longROProp_p;
		CORBA::Long m_longROVal;
		ACS::Time m_time3;
		DevIO<CORBA::Long> *m_longRODevIO;
		baci::ROuLong *m_uLongROProp_p;
		ACS::uLong m_uLongROVal;
		ACS::Time m_time4;
		DevIO<ACS::uLong> *m_uLongRODevIO;
		baci::ROlongLong *m_longLongROProp_p;
		ACS::longLong m_longLongROVal;
		ACS::Time m_time5;
		DevIO<ACS::longLong> *m_longLongRODevIO;
		baci::ROuLongLong *m_uLongLongROProp_p;
		ACS::uLongLong m_uLongLongROVal;
		ACS::Time m_time6;
		DevIO<ACS::uLongLong> *m_uLongLongRODevIO;
		baci::ROboolean *m_booleanROProp_p;
		CORBA::Boolean m_booleanROVal;
		ACS::Time m_time7;
		DevIO<CORBA::Boolean> *m_booleanRODevIO;
		baci::ROdoubleSeq *m_doubleSeqROProp_p;
		ACS::doubleSeq m_doubleSeqROVal;
		ACS::Time m_time8;
		DevIO<ACS::doubleSeq> *m_doubleSeqRODevIO;
		baci::ROfloatSeq *m_floatSeqROProp_p;
		ACS::floatSeq m_floatSeqROVal;
		ACS::Time m_time9;
		DevIO<ACS::floatSeq> *m_floatSeqRODevIO;
		baci::ROlongSeq *m_longSeqROProp_p;
		ACS::longSeq m_longSeqROVal;
		ACS::Time m_time10;
		DevIO<ACS::longSeq> *m_longSeqRODevIO;
		baci::ROuLongSeq *m_uLongSeqROProp_p;
		ACS::uLongSeq m_uLongSeqROVal;
		ACS::Time m_time11;
		DevIO<ACS::uLongSeq> *m_uLongSeqRODevIO;
		baci::RObooleanSeq *m_booleanSeqROProp_p;
		ACS::booleanSeq m_booleanSeqROVal;
		ACS::Time m_time12;
		DevIO<ACS::booleanSeq> *m_booleanSeqRODevIO;
		ROEnumImpl<ACS_ENUM_T(EnumTest), POA_TMCDB::ROEnumTest> *m_EnumTestROProp_p;
		EnumTest m_EnumTestROVal;
		ACS::Time m_time13;
		DevIO<EnumTest> *m_EnumTestRODevIO;

		/**
		 * ALMA C++ coding standards state copy operators should be disabled.
		 */
		void operator=(const MCtestAlarmsComponentImpl&);
	};//class MCtestAlarmsComponentImpl
};//namespace TMCDB


#endif /*!_H*/

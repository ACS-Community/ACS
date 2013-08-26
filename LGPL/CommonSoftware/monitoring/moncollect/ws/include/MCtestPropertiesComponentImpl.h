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
* "@(#) $Id: MCtestPropertiesComponentImpl.h,v 1.2 2012/10/10 09:48:54 bjeram Exp $"
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
#include <baciROpattern.h>
#include <baciROstring.h>
#include <baciROlongLong.h>
#include <baciROuLongLong.h>
#include <baciROboolean.h>
#include <baciROdoubleSeq.h>
#include <baciROfloatSeq.h>
#include <baciROlongSeq.h>
#include <baciROuLongSeq.h>
#include <baciRObooleanSeq.h>
//#include <baciROstringSeq.h>
#include <enumpropROImpl.h>
#include <baciRWdouble.h>
#include <baciRWfloat.h>
#include <baciRWlong.h>
#include <baciRWuLong.h>
#include <baciRWpattern.h>
#include <baciRWstring.h>
#include <baciRWlongLong.h>
#include <baciRWuLongLong.h>
#include <baciRWboolean.h>
#include <baciRWdoubleSeq.h>
#include <baciRWfloatSeq.h>
#include <baciRWlongSeq.h>
#include <baciRWuLongSeq.h>
#include <baciRWbooleanSeq.h>
#include <enumpropRWImpl.h>

namespace TMCDB
{

	/**
	 * Monitor collector implementation
	 */
	class MCtestPropertiesComponentImpl :
		public baci::CharacteristicComponentImpl,
		public POA_TMCDB::MCtestPropertiesComponent
	{
	  public:
		MCtestPropertiesComponentImpl(const ACE_CString& name,
				maci::ContainerServices * containerServices);
		~MCtestPropertiesComponentImpl();
	
		// componnet's life cycle
		void execute();
		void cleanUp();
	
		// implementations of IDL's methods
		ACS::ROdouble_ptr doubleROProp();
		ACS::ROfloat_ptr floatROProp();
		ACS::ROlong_ptr longROProp();
		ACS::ROuLong_ptr uLongROProp();
		ACS::ROpattern_ptr patternROProp();
		ACS::ROstring_ptr stringROProp();
		ACS::ROlongLong_ptr longLongROProp();
		ACS::ROuLongLong_ptr uLongLongROProp();
		ACS::ROboolean_ptr booleanROProp();
		ACS::ROdoubleSeq_ptr doubleSeqROProp();
		ACS::ROfloatSeq_ptr floatSeqROProp();
		ACS::ROlongSeq_ptr longSeqROProp();
		ACS::ROuLongSeq_ptr uLongSeqROProp();
		ACS::RObooleanSeq_ptr booleanSeqROProp();
		ROEnumTest_ptr EnumTestROProp();
		ACS::RWdouble_ptr doubleRWProp();
		ACS::RWfloat_ptr floatRWProp();
		ACS::RWlong_ptr longRWProp();
		ACS::RWuLong_ptr uLongRWProp();
		ACS::RWpattern_ptr patternRWProp();
		ACS::RWstring_ptr stringRWProp();
		ACS::RWlongLong_ptr longLongRWProp();
		ACS::RWuLongLong_ptr uLongLongRWProp();
		ACS::RWboolean_ptr booleanRWProp();
		ACS::RWdoubleSeq_ptr doubleSeqRWProp();
		ACS::RWfloatSeq_ptr floatSeqRWProp();
		ACS::RWlongSeq_ptr longSeqRWProp();
		ACS::RWuLongSeq_ptr uLongSeqRWProp();
		ACS::RWbooleanSeq_ptr booleanSeqRWProp();
		RWEnumTest_ptr EnumTestRWProp();
	
		void reset();
	
	  private:
		//RO
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
		baci::ROpattern *m_patternROProp_p;
		ACS::pattern m_patternROVal;
		ACS::Time m_time5;
		DevIO<ACS::pattern> *m_patternRODevIO;
		baci::ROstring *m_stringROProp_p;
		ACE_CString m_stringROVal;
		ACS::Time m_time6;
		DevIO<ACE_CString> *m_stringRODevIO;
		baci::ROlongLong *m_longLongROProp_p;
		ACS::longLong m_longLongROVal;
		ACS::Time m_time7;
		DevIO<ACS::longLong> *m_longLongRODevIO;
		baci::ROuLongLong *m_uLongLongROProp_p;
		ACS::uLongLong m_uLongLongROVal;
		ACS::Time m_time8;
		DevIO<ACS::uLongLong> *m_uLongLongRODevIO;
		baci::ROboolean *m_booleanROProp_p;
		CORBA::Boolean m_booleanROVal;
		ACS::Time m_time9;
		DevIO<CORBA::Boolean> *m_booleanRODevIO;
		baci::ROdoubleSeq *m_doubleSeqROProp_p;
		ACS::doubleSeq m_doubleSeqROVal;
		ACS::Time m_time10;
		DevIO<ACS::doubleSeq> *m_doubleSeqRODevIO;
		baci::ROfloatSeq *m_floatSeqROProp_p;
		ACS::floatSeq m_floatSeqROVal;
		ACS::Time m_time11;
		DevIO<ACS::floatSeq> *m_floatSeqRODevIO;
		baci::ROlongSeq *m_longSeqROProp_p;
		ACS::longSeq m_longSeqROVal;
		ACS::Time m_time12;
		DevIO<ACS::longSeq> *m_longSeqRODevIO;
		baci::ROuLongSeq *m_uLongSeqROProp_p;
		ACS::uLongSeq m_uLongSeqROVal;
		ACS::Time m_time13;
		DevIO<ACS::uLongSeq> *m_uLongSeqRODevIO;
		baci::RObooleanSeq *m_booleanSeqROProp_p;
		ACS::booleanSeq m_booleanSeqROVal;
		ACS::Time m_time14;
		DevIO<ACS::booleanSeq> *m_booleanSeqRODevIO;
		ROEnumImpl<ACS_ENUM_T(EnumTest), POA_TMCDB::ROEnumTest> *m_EnumTestROProp_p;
		EnumTest m_EnumTestROVal;
		ACS::Time m_time15;
		DevIO<EnumTest> *m_EnumTestRODevIO;
		//RW
		baci::RWdouble *m_doubleRWProp_p;
		CORBA::Double m_doubleRWVal;
		ACS::Time m_time16;
		DevIO<CORBA::Double> *m_doubleRWDevIO;
		baci::RWfloat *m_floatRWProp_p;
		CORBA::Float m_floatRWVal;
		ACS::Time m_time17;
		DevIO<CORBA::Float> *m_floatRWDevIO;
		baci::RWlong *m_longRWProp_p;
		CORBA::Long m_longRWVal;
		ACS::Time m_time18;
		DevIO<CORBA::Long> *m_longRWDevIO;
		baci::RWuLong *m_uLongRWProp_p;
		ACS::uLong m_uLongRWVal;
		ACS::Time m_time19;
		DevIO<ACS::uLong> *m_uLongRWDevIO;
		baci::RWpattern *m_patternRWProp_p;
		ACS::pattern m_patternRWVal;
		ACS::Time m_time20;
		DevIO<ACS::pattern> *m_patternRWDevIO;
		baci::RWstring *m_stringRWProp_p;
		ACE_CString m_stringRWVal;
		ACS::Time m_time21;
		DevIO<ACE_CString> *m_stringRWDevIO;
		baci::RWlongLong *m_longLongRWProp_p;
		ACS::longLong m_longLongRWVal;
		ACS::Time m_time22;
		DevIO<ACS::longLong> *m_longLongRWDevIO;
		baci::RWuLongLong *m_uLongLongRWProp_p;
		ACS::uLongLong m_uLongLongRWVal;
		ACS::Time m_time23;
		DevIO<ACS::uLongLong> *m_uLongLongRWDevIO;
		baci::RWboolean *m_booleanRWProp_p;
		CORBA::Boolean m_booleanRWVal;
		ACS::Time m_time24;
		DevIO<CORBA::Boolean> *m_booleanRWDevIO;
		baci::RWdoubleSeq *m_doubleSeqRWProp_p;
		ACS::doubleSeq m_doubleSeqRWVal;
		ACS::Time m_time25;
		DevIO<ACS::doubleSeq> *m_doubleSeqRWDevIO;
		baci::RWfloatSeq *m_floatSeqRWProp_p;
		ACS::floatSeq m_floatSeqRWVal;
		ACS::Time m_time26;
		DevIO<ACS::floatSeq> *m_floatSeqRWDevIO;
		baci::RWlongSeq *m_longSeqRWProp_p;
		ACS::longSeq m_longSeqRWVal;
		ACS::Time m_time27;
		DevIO<ACS::longSeq> *m_longSeqRWDevIO;
		baci::RWuLongSeq *m_uLongSeqRWProp_p;
		ACS::uLongSeq m_uLongSeqRWVal;
		ACS::Time m_time28;
		DevIO<ACS::uLongSeq> *m_uLongSeqRWDevIO;
		baci::RWbooleanSeq *m_booleanSeqRWProp_p;
		ACS::booleanSeq m_booleanSeqRWVal;
		ACS::Time m_time29;
		DevIO<ACS::booleanSeq> *m_booleanSeqRWDevIO;
		RWEnumImpl<ACS_ENUM_T(EnumTest), POA_TMCDB::RWEnumTest> *m_EnumTestRWProp_p;
		EnumTest m_EnumTestRWVal;
		ACS::Time m_time30;
		DevIO<EnumTest> *m_EnumTestRWDevIO;
		/**
		 * ALMA C++ coding standards state copy operators should be disabled.
		 */
		void operator=(const MCtestPropertiesComponentImpl&);
	
	};//class MCtestComponentImpl

};//namespace TMCDB


#endif /*!_H*/

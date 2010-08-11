#ifndef SAMPLEDCOMP_IMPL_H
#define SAMPLEDCOMP_IMPL_H
/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) Universidad Tecnica Federico Santa Maria, 2008
*    Copyright by UTFSM (in the framework of the ALMA collaboration)
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
*
* "@(#) $Id: sampledCompImpl.h,v 1.10 2010/08/11 15:03:10 ntroncos Exp $"
*
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <string>

#include <acscomponentImpl.h>
#include <baciCharacteristicComponentImpl.h>
#include <baciSmartPropertyPointer.h>

#include <baciRWdouble.h>
#include <baciRWfloat.h>
#include <baciRWstring.h>
#include <baciRWlong.h>
#include <baciRWlongLong.h>
#include <baciROdouble.h>
#include <baciROfloat.h>
#include <baciROstring.h>
#include <baciROlong.h>
#include <baciROlongLong.h>

#include "sampledCompS.h"

using namespace baci;

class sampledCompImpl: public virtual CharacteristicComponentImpl,
		     public virtual POA_SAMP::SampledComponent
{

	public:
		sampledCompImpl(const ACE_CString &name, maci::ContainerServices *containerServices);

		/**
		 * Destructor
		 */
		virtual ~sampledCompImpl();
		void initialize();

		virtual ACS::RWdouble_ptr   my_RWdouble();
		virtual ACS::ROdouble_ptr   my_ROdouble();
		virtual ACS::RWfloat_ptr    my_RWfloat();
		virtual ACS::ROfloat_ptr    my_ROfloat();
		virtual ACS::RWstring_ptr   my_RWstring();
		virtual ACS::ROstring_ptr   my_ROstring();
		virtual ACS::RWlong_ptr     my_RWlong();
		virtual ACS::ROlong_ptr     my_ROlong();
		virtual ACS::RWlongLong_ptr my_RWlongLong();
		virtual ACS::ROlongLong_ptr my_ROlongLong();

	private:
		SmartPropertyPointer<RWdouble>   m_RWdouble_sp;
		SmartPropertyPointer<ROdouble>   m_ROdouble_sp;
		SmartPropertyPointer<RWfloat>    m_RWfloat_sp;
		SmartPropertyPointer<ROfloat>    m_ROfloat_sp;
		SmartPropertyPointer<RWstring>   m_RWstring_sp;
		SmartPropertyPointer<ROstring>   m_ROstring_sp;
		SmartPropertyPointer<RWlong>     m_RWlong_sp;
		SmartPropertyPointer<ROlong>     m_ROlong_sp;
		SmartPropertyPointer<RWlongLong> m_RWlongLong_sp;
		SmartPropertyPointer<ROlongLong> m_ROlongLong_sp;
		ACE_CString component_name;
	/**
	 * ALMA C++ coding standards state copy operators should be disabled.
	 */
	void operator=(const sampledCompImpl&);

};

#endif /* !SAMPLEDCOMP_IMPL_H */

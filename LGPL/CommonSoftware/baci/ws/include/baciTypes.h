#ifndef baciTypes_H
#define baciTypes_H

/*******************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2004 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: baciTypes.h,v 1.99 2012/10/09 14:22:58 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/03/03  modified
*/

/** 
 * @file 
 * Header file for BACI Types.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciC.h>
#include <baciS.h>

#include <acscommonC.h>

/** @defgroup BACISupportedTypesTypedefs BACI Support Types (C++ Typedefs on Types defined in baci.idl)
 * The following list of typedefs is used to make BACI code IDL independent. It should be noted that
 * the "ACS" namespace derived from baci.idl is removed in each and every one of these typedefs and
 * also that these definitions do not even exist in the "baci" namespace. Instead each typedef has the 
 * string "BACI" prepended to it's name. For example, "ACS::pattern" becomes "BACIPattern".
 *  @{
 */
typedef const char* BACIstring;
typedef ACS::pattern BACIpattern;

typedef CORBA::Double BACIdouble;
typedef CORBA::Float BACIfloat;
typedef CORBA::Long BACIlong;
typedef ACS::uLong BACIuLong;
typedef CORBA::Boolean BACIboolean;

typedef ACS::doubleSeq BACIdoubleSeq;
typedef ACS::floatSeq BACIfloatSeq;
typedef ACS::longSeq BACIlongSeq;
typedef ACS::uLongSeq BACIuLongSeq;
typedef ACS::booleanSeq BACIbooleanSeq;

typedef ACS::longLong BACIlongLong;
typedef ACS::uLongLong BACIuLongLong;

typedef ACS::stringSeq BACIstringSeq;

typedef ACS::Callback Callback;
typedef ACS::Callback_ptr Callback_ptr;
typedef ACS::CBDescIn CBDescIn;
typedef ACS::CBDescOut CBDescOut;
typedef ACSErr::Completion Completion;
typedef ACS::CBvoid CBvoid;
typedef ACS::CBdouble CBdouble;
typedef ACS::CBfloat CBfloat;
typedef ACS::CBlong CBlong;
typedef ACS::CBuLong CBuLong;
typedef ACS::CBlongLong CBlongLong;
typedef ACS::CBuLongLong CBuLongLong;
typedef ACS::CBstring CBstring;
typedef ACS::CBstringSeq CBstringSeq;
typedef ACS::CBpattern CBpattern;
typedef ACS::CBboolean CBboolean;
typedef ACS::CBvoid_var CBvoid_var;
typedef ACS::CBdoubleSeq CBdoubleSeq;
typedef ACS::CBfloatSeq CBfloatSeq;
typedef ACS::CBlongSeq CBlongSeq;
typedef ACS::CBuLongSeq CBuLongSeq;
typedef ACS::CBbooleanSeq CBbooleanSeq;
typedef ACS::CBdouble_var CBdouble_var;
typedef ACS::CBfloat_var CBfloat_var;
typedef ACS::CBlong_var CBlong_var;
typedef ACS::CBuLong_var CBuLong_var;
typedef ACS::CBlongLong_var CBlongLong_var;
typedef ACS::CBuLongLong_var CBuLongLong_var;
typedef ACS::CBstring_var CBstring_var;
typedef ACS::CBpattern_var CBpattern_var;
typedef ACS::CBboolean_var CBboolean_var;
typedef ACS::CBdoubleSeq_var CBdoubleSeq_var;
typedef ACS::CBfloatSeq_var CBfloatSeq_var;
typedef ACS::CBlongSeq_var CBlongSeq_var;
typedef ACS::CBuLongSeq_var CBuLongSeq_var;
typedef ACS::CBstringSeq_var CBstringSeq_var;
typedef ACS::CBbooleanSeq_var CBbooleanSeq_var;
typedef ACS::CharacteristicComponent CharacteristicComponent;

typedef acscommon::TimeStamp BACITimeStamp;
typedef ACS::TimeInterval BACITimeInterval;
/**
 * @}
 */

#endif /* baciTypes_H */


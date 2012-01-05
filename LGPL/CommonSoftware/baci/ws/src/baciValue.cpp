/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration),
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
* "@(#) $Id: baciValue.cpp,v 1.112 2012/01/05 14:46:55 rtobar Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    2003/01/08  since gcc 2.7.2 (vxWorks) does not support calls like Value<double>() I've changed accessor function to T getValue(T*) and I've also changed mutator function to setValue (to be consistent)
* bjeram    2002/12/20  added template implementation for mutator and accessor functions (tmp)
* msekoran  2001/07/26  fixed string type
* msekoran  2001/03/03  created
*/

#include <vltPort.h>

#include "baciValue.h"
#include <sstream>
#include <string>

#include "logging.h"

ACE_RCSID(baci, baciValue, "$Id: baciValue.cpp,v 1.112 2012/01/05 14:46:55 rtobar Exp $")

 using namespace baci;


#define ACCESSOR_INLINE_TYPE(ty, realType)     \
realType BACIValue::getValue(realType *n)  const          \
{                                              \
  if (type_m != type_##ty ){                      \
    n = 0; \
    return 0;                                  \
  } else { \
    if (n) *n = *(( realType *)inlineData_m);           \
    return *(( realType *)inlineData_m);         \
  } \
}

#define ACCESSOR_PTR_TYPE(ty, realType)        \
realType BACIValue::getValue(realType *n) const \
{                                              \
  if (type_m != type_##ty ){                      \
    n = 0; \
    return realType();                                  \
  }else{                                         \
    if (n) *n = *(( realType *)ptr_m.pointer); \
    return *(( realType *)ptr_m.pointer);        \
  } \
}


ACE_CString BACIValue::getValue(ACE_CString *n) const
{
  if (type_m != type_string ){
    n = 0;
    return ACE_CString();
  }else{
    if (n) *n = ACE_CString( (( ACE_TCHAR *)ptr_m.pointer) );
    return ACE_CString( (( ACE_TCHAR *)ptr_m.pointer) );
  }
}

ACCESSOR_INLINE_TYPE(double, BACIdouble)
ACCESSOR_INLINE_TYPE(float, BACIfloat)
ACCESSOR_INLINE_TYPE(long, BACIlong)
ACCESSOR_INLINE_TYPE(longLong, BACIlongLong)
ACCESSOR_INLINE_TYPE(uLongLong, BACIuLongLong)
//TOBE deleted ACCESSOR_INLINE_TYPE(pattern, BACIpattern)

//ACCESSOR_PTR_TYPE(string, ACE_CString)
ACCESSOR_PTR_TYPE(doubleSeq, BACIdoubleSeq)
ACCESSOR_PTR_TYPE(floatSeq, BACIfloatSeq)
ACCESSOR_PTR_TYPE(longSeq, BACIlongSeq)
ACCESSOR_PTR_TYPE(stringSeq, BACIstringSeq)

#undef ACCESSOR_PTR_TYPE
#undef ACCESSOR_INLINE_TYPE
// ----------------------------[ Mutators ]--------------------------------

#define MUTATOR_INLINE_TYPE(ty, realType)                 \
bool BACIValue::setValue(const realType &value) \
{                                                         \
  if (type_m != type_##ty )                                 \
    if (!setType(type_##ty))                              \
       return false;                                      \
                                                          \
  *(( realType *)inlineData_m) = value;                     \
  return true;                                            \
}

#define MUTATOR_PTR_TYPE(ty, realType)                    \
bool BACIValue::setValue(const realType &value) \
{                                                         \
  if (type_m != type_##ty )                                 \
    if (!setType(type_##ty))                              \
       return false;                                      \
                                                          \
  *(( realType *)ptr_m.pointer) = value;                    \
  return true;                                            \
}

bool BACIValue::setValue(const ACE_CString &value)
{
  if (type_m != type_string )
    if (!setType(type_string))
       return false;

  if (ptr_m.pointer) delete (ACE_TCHAR*)ptr_m.pointer;
  //ptr_m.pointer = new ACE_CString(value);
  ACE_TCHAR * str;// = new ACE_TCHAR[ACE_OS::strlen(value.length())+1];
  str = value.rep();
  ptr_m.pointer = str;

  return true;
}
MUTATOR_INLINE_TYPE(double, BACIdouble)
MUTATOR_INLINE_TYPE(float, BACIfloat)
MUTATOR_INLINE_TYPE(long, BACIlong)
MUTATOR_INLINE_TYPE(longLong, BACIlongLong)
MUTATOR_INLINE_TYPE(uLongLong, BACIuLongLong)
//TOBE deleted MUTATOR_INLINE_TYPE(pattern, BACIpattern)

//MUTATOR_PTR_TYPE(string, ACE_CString)
MUTATOR_PTR_TYPE(doubleSeq, BACIdoubleSeq)
MUTATOR_PTR_TYPE(floatSeq, BACIfloatSeq)
MUTATOR_PTR_TYPE(longSeq, BACIlongSeq)
MUTATOR_PTR_TYPE(stringSeq, BACIstringSeq)

#undef MUTATOR_PTR_TYPE
#undef MUTATOR_INLINE_TYPE

const BACIValue BACIValue::NullValue(BACIValue::type_null, VALUE_NULL);

const ACE_CString BACIValue::typeName[] = {
    "null",
    "pointer",
    "string",
    "double",
    "long",
    "pattern",
    "doubleSeq",
    "longSeq",
    "longLong",
    "uLongLong",
    "stringSeq",
    "float",
    "floatSeq",
    "enum"
  };

/// Archive support only three types (long, double, string)
const ACE_CString BACIValue::archiveTypeName[] = {
    "invalid",
    "invalid",
    "string",
    "double",
    "long",
    "pattern",
    "doubleSeq",   // !!! not supported by logging (yet)
    "longSeq",
    "longLong",
    "uLongLong",
    "stringSeq",
    "float",
    "floatSeq",
    "enum"
  };

// ----------------------------------------------------------------------

#define OUTPUT_INLINE_TYPE(ty, realType)                                     \
  case ( type_##ty ):                                                        \
    if (specifyType) os << "<" << BACIValue::typeName[ type_##ty ] << ">";   \
    (ostream&)os << *(( realType *)inlineData_m) << '\0';                      \
    value = os.str().c_str();                                      \
    return true;

#define OUTPUT_PTR_TYPE_WITH_BOUND(ty, realType)          \
  case ( type_##ty ):                                     \
    if (specifyType)                                      \
       {                                                  \
          os << "<" << BACIValue::typeName[ type_##ty ];  \
          if (ptr_m.bound != 0)                             \
               (ostream&)os << ':' << ptr_m.bound;          \
          os << ">";                                      \
       }                                                  \
    (ostream&)os << *(( realType *)ptr_m.pointer) << '\0';  \
    value = os.str().c_str();                  \
    return true;

#define OUTPUT_PTR_SEQ_TYPE_WITH_BOUND(ty, realType)           \
  case ( type_##ty ):                                          \
    if (specifyType)                                           \
       {                                                       \
          os << "<" << BACIValue::typeName[ type_##ty ];       \
          if ((*( realType *)ptr_m.pointer).length() != 0)                                  \
               (ostream&)os << ':' << ptr_m.bound;               \
          os << ">";                                           \
       }                                                       \
    if ((*( realType *)ptr_m.pointer).length()>1)                                           \
      for (CORBA::ULong i=0; i < (*( realType *)ptr_m.pointer).length(); i++)           \
         { \
         (ostream&)os << (*( realType *)ptr_m.pointer)[i]; \
         if (i != (*( realType *)ptr_m.pointer).length()-1) \
	   { \
	       (ostream&)os << ','; \
	   } \
	 } \
    (ostream&)os << '\0';                                      \
    value = os.str().c_str();                        \
    return true;


//------------------------------------------------------------------
//DWF-all the follows is a nasty hack used to alleviate problems
//with enumerations. Eventually it should be redesigned when there's
//more time and not such an urgency.

CORBA::Any
BACIValue::enumValue() const
{
    return any_m;
}

CORBA::Any
BACIValue::getValue(CORBA::Any *v) const
{
    return any_m;
}

//replaced BACIpatter with int
bool
BACIValue::enumValue(const BACIpattern &value, const CORBA::Any &anyVal)
{
    if (patternValue(value)==true)
	{
	any_m = anyVal;
	isEnum_m = true;
	return true;
	}
    else
	{
	return false;
	}
}
//tobe deleted
/*
bool
BACIValue::setValue(const BACIpattern &value, const CORBA::Any &anyVal)
{
    if (setValue(value)==true)
	{
	any_m = anyVal;
	isEnum_m = true;
	return true;
	}
    else
	{
	return false;
	}
}
*/
//------------------------------------------------------------------

bool BACIValue::toString(ACE_CString &value, bool specifyType) const
{
  //unsigned int i;
  std::ostringstream os;

  switch (type_m)
    {

      /// User defined

      OUTPUT_INLINE_TYPE(double, BACIdouble)
      OUTPUT_INLINE_TYPE(float, BACIfloat)
      OUTPUT_INLINE_TYPE(long, BACIlong)
      OUTPUT_INLINE_TYPE(longLong, BACIlongLong)
      OUTPUT_INLINE_TYPE(uLongLong, BACIuLongLong)
//TBDeleted      OUTPUT_INLINE_TYPE(pattern, BACIpattern)
      OUTPUT_PTR_TYPE_WITH_BOUND(string, BACIstring)
      OUTPUT_PTR_SEQ_TYPE_WITH_BOUND(doubleSeq, BACIdoubleSeq)
      OUTPUT_PTR_SEQ_TYPE_WITH_BOUND(floatSeq, BACIfloatSeq)
      OUTPUT_PTR_SEQ_TYPE_WITH_BOUND(longSeq, BACIlongSeq)
      OUTPUT_PTR_SEQ_TYPE_WITH_BOUND(stringSeq, BACIstringSeq)

    default:
      return false;
    }

}

#undef OUTPUT_INLINE_TYPE
#undef OUTPUT_PTR_TYPE_WITH_BOUND

// ----------------------------------------------------------------------

#define PROCESS_INLINE_TYPE(ty, realType)                      \
  if(strType.compare(BACIValue::typeName[ type_##ty ]) == 0)   \
    {                                                          \
      std::istringstream is(szContent);                        \
      realType tmp;                                            \
      (istream&) is >> tmp;                                    \
      if (!setType( type_##ty ))                               \
	return false;                                          \
      return ty##Value(tmp);                                   \
    }

bool BACIValue::fromString(const ACE_CString value, bool specifyType)
{

  ACE_CString strType;
  ACE_CString strContent;
  const char *szType;
  const char *szContent;
  unsigned long ulBound = 0;

  if (specifyType)
    {
      ACE_CString::size_type nPos0 = value.find('<');
      ACE_CString::size_type nPos1 = value.find(':');
      ACE_CString::size_type nPos2 = value.find('>');

      if((nPos1 != ACE_CString::npos) && (nPos1 < (nPos2-1)))
		ulBound = atoi(value.substr(nPos1+1, nPos2-nPos1-1).c_str());
      else
		nPos1 = nPos2;

      strType = value.substr(nPos0+1, nPos1-nPos0-1);
      strContent = value.substr(nPos2+1);
      szType = strType.c_str();
      szContent = strContent.c_str();

    }
  else
    {
      strType = typeName[type_m];
      strContent = value;
      szType = strType.c_str();
      szContent = strContent.c_str();
    }

/// User defined

  // special threathment for string (no conversion needed)
  if(strType.compare(BACIValue::typeName[type_string]) == 0)
    {
      if ((ulBound != 0) && (strContent.length() > ulBound))
	return false;
      if(!setType(type_string, ulBound))
	return 0;
      return stringValue(szContent);
    }

   PROCESS_INLINE_TYPE(double, BACIdouble)
   PROCESS_INLINE_TYPE(float, BACIfloat)
   PROCESS_INLINE_TYPE(long, BACIlong)
   PROCESS_INLINE_TYPE(longLong, BACIlongLong)
   PROCESS_INLINE_TYPE(uLongLong, BACIuLongLong)
//TBDeleted   PROCESS_INLINE_TYPE(pattern, BACIpattern)

  return false;

}

#undef PROCESS_INLINE_TYPE

// ------------------------------------------------------------------------
void
BACIValue::getAny(CORBA::Any &any) const
{
    switch(type_m)
	{
	// No-value. Can also be used to imply special meaning.
	case type_null:
	    //in this case - it's safe to do nothing and tk_null will be
	    //used
	    break;

	    // Not really a type. This is just a void pointer used to pass any user-defined
	    // structures as a BACI Action parameter. <b>THIS COULD NOT BE USED AS BACI TYPE!</b>
	case type_pointer:
	    //in this case - it's safe to do nothing and tk_null will be
	    //used
	    break;

	    // A string.
	case type_string:
	    any <<= this->getValue((char **)0);
	    break;

	    // A IEEE 8-byte floating point number.
	case type_double:
	    any <<= this->doubleValue();
	    break;

	    // 32-bit signed integer.
	case type_long:
	    any <<= this->longValue();
	    break;

	    // A bit former pattern.
	case type_uLongLong /*type_pattern*/:
	    //special case because this holds enums as well
	    if (isEnum_m==true)
		{
		//it's already in any format.
		any = any_m;
		}
	    else
		{
		any <<= this->patternValue();
		}

	    break;

	    // Sequence of double-s.
	case type_doubleSeq:
	    any <<= this->doubleSeqValue();
	    break;

	    // Sequencs of long-s.
	case type_longSeq:
	    any <<= this->longSeqValue();
	    break;

	    // 64-bit signed integer.
	case type_longLong:
	    any <<= this->longLongValue();
	    break;

/*TBdeleted	    // 64-bit unsigned integer.
	case type_uLongLong:
	    any <<= this->uLongLongValue();
	    break;
*/
	    // Sequence of string-s.
	case type_stringSeq:
	    any <<= this->stringSeqValue();
	    break;

	    // A IEEE 4-byte floating point number.
	case type_float:
	    any <<= this->floatValue();
	    break;

	    // Sequence of float-s.
	case type_floatSeq:
	    any <<= this->floatSeqValue();
	    break;

	    // Have no idea how this could occur but just in case...
	default:
	    ACS_SHORT_LOG((LM_ERROR, "BACIValue::getAny - type not supported!"));
	    break;

	}

    return;
}


// ------------------[ Stream extraction ]-----------------------

std::istream& operator>>(std::istream &is, ACE_CString &data)
{

#ifndef MAKE_VXWORKS
// for some reason does not work for VxWorks !!!!
    // get length
    is.seekg (0, ios::end);
    int length = is.tellg();
    is.seekg (0, ios::beg);

    // if empty length == -1 and bad bit is set
    if (length == -1)
      {
        data = "";
        // clear bad bits
        is.clear();
        return is;
      }

#else
    int length = 512;
#endif

    // allocate memory:
    char * buffer = new char [length+1];

    // read data as a block:
    is.read (buffer,length);
    buffer[length] = 0;

    // copy
    data = (const char*)buffer;
#ifdef MAKE_VXWORKS
    is.clear();
#endif
    // delete buffer
    delete buffer;

  return is;
}

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: baciValue.cpp,v $
// Revision 1.112  2012/01/05 14:46:55  rtobar
// Final touches to let baci compiling without warnings (32/64 bits)
//
// Revision 1.111  2008/12/29 08:34:43  bjeram
// aligned BACIValue::typeName and BACIValue::archiveTypeName with BACIValue::Type enum.
//
// Revision 1.110  2008/08/21 15:30:52  bjeram
// after increasing size of pattern from 32 to 64 bits (COMP-2146) there is not anymore difference between unsingle long long and patter
//
// Revision 1.109  2006/09/01 02:20:54  cparedes
// small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
//
// Revision 1.108  2005/12/15 19:20:31  dfugate
// If the BACIValue is null or a pointer to a C++ object, do not store anything
// into the CORBA any. By default, it should be initialized to tk_null and the
// CORBA::Object::_nil() reference that was being sent seemed to confuse people.
//
// Revision 1.107  2005/11/17 12:19:50  msekoran
// isstreamstream behaviour when string is emptry fixed.
//
// Revision 1.106  2005/11/03 19:53:15  dfugate
// Added indirect support for enumerations within the BACIValue class.
// This is only a temporary solution to close an SPR which relates to
// enum property values being cast to ACS::patterns before being sent
// to the ALMA archive. After ACS 5.0 is released
// some serious thought should be put into a better solution (such as completely
// reimplementing the BACIValue class using templates or using CORBA::Anys) as
// these changes are not suitable for long term use.
//
// Revision 1.105  2005/10/07 13:10:24  bjeram
// fixed istream to ACS_CString converter that it works for VxWorks as well
//
// Revision 1.104  2005/09/28 13:53:09  msekoran
// New CDB code.
//
// Revision 1.103  2005/09/12 23:02:54  dfugate
// Added getAny method.
//
// Revision 1.102  2005/09/12 19:55:38  dfugate
// Files were ending in Windows newline characters. Fixed.
//
// Revision 1.101  2005/08/23 15:37:16  vwang
// add ROfloat RWfloat ROfloatSeq RWfloatSeq into BACI
//
// Revision 1.100  2005/04/27 21:33:27  dfugate
// toString method broken for sequence types.
//
// Revision 1.99  2005/02/01 00:44:38  dfugate
// Fixed more C++ coding standard violations.
//
// Revision 1.98  2005/01/26 00:13:56  dfugate
// BACIValue is now completely documented in terms of Doxygen comments and macros have been removed from the header.
//
// Revision 1.97  2005/01/07 18:18:02  dfugate
// Removed REVISION sections where there was no meaningful information. This is redundant as "cvs rlog" provides this as well as the comments below the LGPL disclaimer.
//
// Revision 1.96  2004/06/21 07:59:28  bjeram
// refacroting Seq impl + add SeqDis + ROStringSeq
//
// Revision 1.95  2003/09/10 17:01:08  gchiozzi
// Archived check point after big ACS 3.0 refactoring.
// Now everything compiles, but still the tests are not working.
//



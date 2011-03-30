/*******************************************************************************
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
* "@(#) $Id: baciValue.i,v 1.109 2011/03/30 17:57:23 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2003-02-13 strings are (should) be hold as ACE_CString (setType has been changed to delete ACE_CString and BACIValue constructor for const char* has been changed too)
* almamgr 2002-02-05 Removed ## concatenation characters in macros.
* almamgr 2002-02-05 created
* msekoran  2001/07/26  fixed string type
* msekoran  2001/03/02  created
*/

// --------------------------[ Constructors ]------------------------------

inline BACIValue::BACIValue() :
    type_m(type_null),
    whyNull_m(VALUE_UNINITIALIZED),
    isEnum_m(false)
{
  ptr_m.bound = 0;
  ptr_m.pointer = 0;
}

inline BACIValue::BACIValue(const BACIValue::Type type, const unsigned long bound) :
  type_m(type_null),
  whyNull_m(VALUE_UNINITIALIZED),
  isEnum_m(false)
{
  ptr_m.bound = 0;
  ptr_m.pointer = 0;
  setType(type, bound);
}

inline BACIValue::BACIValue(const BACIValue &value) :
  type_m(type_null), whyNull_m(VALUE_UNINITIALIZED)
{
  ptr_m.bound = 0;
  ptr_m.pointer = 0;

  // Delegate to the copy operator.
  *this = value;
}

inline void BACIValue::reset()
{
  // destroy and reinitialize
  setType(type_null);
  isEnum_m = false;
  whyNull_m = VALUE_UNINITIALIZED;
  ptr_m.bound = 0;
  ptr_m.pointer = 0;
}

/**
 * Helper macro creates the constructor implementation.
 */
#define CONSTRUCTOR(ty, realType)                    \
inline BACIValue::BACIValue(const realType &value) : \
  type_m( type_##ty ),                                  \
  isEnum_m(false)                                   \
{                                                    \
  ptr_m.bound = 0;                                     \
  ptr_m.pointer = 0;                                   \
  setType( type_##ty );                              \
  ty##Value(value);                                  \
}

/// User defined

inline BACIValue::BACIValue(const char *value) :
    type_m(type_string),
    isEnum_m(false)
{
  ptr_m.bound = 0;
  ptr_m.pointer = 0;
  setType(type_string);
  stringValue(value);
//  setValue(ACE_CString(value));
}


inline
BACIValue::BACIValue(const ACE_CString &value) :
    type_m(type_string),
    isEnum_m(false)
{
  ptr_m.bound = 0;
  ptr_m.pointer = 0;
  setType(type_string);
  stringValue(value.c_str());
}

inline BACIValue::BACIValue(const BACIpattern &value, const CORBA::Any& any) :
    type_m(type_pattern),
    isEnum_m(true)
{
  ptr_m.bound = 0;
  ptr_m.pointer = 0;
  setType(type_pattern);
  patternValue(value);

  any_m = any;
}
CONSTRUCTOR(double, BACIdouble)
CONSTRUCTOR(float, BACIfloat)
CONSTRUCTOR(long, BACIlong)
CONSTRUCTOR(longLong, BACIlongLong)
CONSTRUCTOR(uLongLong, BACIuLongLong)
//TOBE deleted CONSTRUCTOR(pattern, BACIpattern)
//CONSTRUCTOR(string, BACIstring)

inline BACIValue::BACIValue(void * value) :
    type_m(type_pointer),
    isEnum_m(false)
{
  setType(type_pointer);
  pointerValue(value);
}

CONSTRUCTOR(doubleSeq, BACIdoubleSeq)
CONSTRUCTOR(floatSeq, BACIfloatSeq)
CONSTRUCTOR(longSeq, BACIlongSeq)
CONSTRUCTOR(stringSeq, BACIstringSeq)

#undef CONSTRUCTOR

// ---------------------------[ Destructor ]-------------------------------

inline BACIValue::~BACIValue()
{
  setType(type_null);
}

// -----------------[ Set type, assignment operators ]----------------------
/**
 * Helper macro.
 */
#define DESTROY_PTR_TYPE(ty, realType)  \
    case (type_##ty):                   \
      if (ptr_m.pointer != 0)                  \
        delete static_cast<realType *>(ptr_m.pointer);\
      break;
/**
 * Helper macro.
 */
#define CREATE_INLINE_TYPE(ty, realType)  \
    case ( type_##ty ):                   \
      new(inlineData_m) realType;               \
      break;
/**
 * Helper macro.
 */
#define CREATE_PTR_TYPE(ty, realType)           \
    case ( type_##ty ):                         \
      if ((ptr_m.pointer = new realType ()) == 0) \
        return false;                           \
      break;

inline bool BACIValue::setType(Type _type, unsigned long _bound)
{
  // release if needed
  if(type_m != type_null)
    {
      switch(type_m)
        {

    case (type_string):
      if (ptr_m.pointer != 0)
        delete[] static_cast<ACE_TCHAR *>(ptr_m.pointer);
      break;

/// User defined
DESTROY_PTR_TYPE(doubleSeq, BACIdoubleSeq)
DESTROY_PTR_TYPE(floatSeq, BACIfloatSeq)
DESTROY_PTR_TYPE(longSeq, BACIlongSeq)
DESTROY_PTR_TYPE(stringSeq, BACIstringSeq)

	default:
	  break;
        }
    }

  // set & allocate if needed
  type_m = _type;
  if(type_m == type_null)
    {
      whyNull_m = _bound;
      return true;
    }

  ptr_m.bound = _bound;
  switch(type_m)
    {
    case type_null:
      memset(inlineData_m, 0x00, sizeof(inlineData_m));
      break;
    case type_pointer:
    case type_string:
      ptr_m.pointer = 0;
      break;

/// User defined

CREATE_INLINE_TYPE(double, BACIdouble)
CREATE_INLINE_TYPE(float, BACIfloat)
CREATE_INLINE_TYPE(long, BACIlong)
CREATE_INLINE_TYPE(longLong, BACIlongLong)
CREATE_INLINE_TYPE(uLongLong, BACIuLongLong)
//TOBE deleted CREATE_INLINE_TYPE(pattern, BACIpattern)

CREATE_PTR_TYPE(doubleSeq, BACIdoubleSeq)
CREATE_PTR_TYPE(floatSeq, BACIfloatSeq)
CREATE_PTR_TYPE(longSeq, BACIlongSeq)
CREATE_PTR_TYPE(stringSeq, BACIstringSeq)

    default:
      return false;
    }

  return true;
}

#undef DESTROY_PTR_TYPE
#undef CREATE_INLINE_TYPE
#undef CREATE_PTR_TYPE

//---------------------------------------------------------------------
/**
 * Helper macro.
 */
#define ASSIGN_INLINE_TYPE(ty, realType)                  \
    case ( type_##ty ):                                   \
      new(inlineData_m) realType (* ( realType *) value.inlineData_m); \
      break;
/**
 * Helper macro.
 */
#define ASSIGN_PTR_TYPE(ty, realType)                              \
    case ( type_##ty ):                                            \
      * (static_cast<realType *>(ptr_m.pointer)) = * (static_cast<realType *>(value.ptr_m.pointer) ); \
      break;

inline BACIValue& BACIValue::operator=(const BACIValue &value)
{
  setType(value.type_m, value.ptr_m.bound);
  isEnum_m = value.isEnum_m;
  any_m    = value.any_m;

  switch(value.type_m)
    {
    case (type_pointer):
      ptr_m.pointer = value.ptr_m.pointer;
      break;

/// User defined
    case (type_string):
	if (value.ptr_m.pointer==0)
	  ptr_m.pointer = 0;
	else
	{
	  if (ptr_m.pointer!=0)
	      {
	      delete[] static_cast<ACE_TCHAR*>(ptr_m.pointer);
	      }
	  ACE_TCHAR * str_p = new ACE_TCHAR[ACE_OS::strlen((const ACE_TCHAR*)value.ptr_m.pointer)+1];
	  ACE_OS::strcpy(str_p, (const ACE_TCHAR*)value.ptr_m.pointer);
	  ptr_m.pointer = str_p;
	}
	break;

ASSIGN_INLINE_TYPE(double, BACIdouble)
ASSIGN_INLINE_TYPE(float, BACIfloat)
ASSIGN_INLINE_TYPE(long, BACIlong)
ASSIGN_INLINE_TYPE(longLong, BACIlongLong)
ASSIGN_INLINE_TYPE(uLongLong, BACIuLongLong)
//TOBE deleted ASSIGN_INLINE_TYPE(pattern, BACIpattern)

ASSIGN_PTR_TYPE(doubleSeq, BACIdoubleSeq)
ASSIGN_PTR_TYPE(floatSeq, BACIfloatSeq)
ASSIGN_PTR_TYPE(longSeq, BACIlongSeq)
ASSIGN_PTR_TYPE(stringSeq, BACIstringSeq)

    default:
      break;
    }

  return *this;
}

#undef ASSIGN_INLINE_TYPE
#undef ASSIGN_PTR_TYPE

// ------------------------------------------------------------------------
/**
 * Helper macro.
 */
#define COMPARE_EQUALS_INLINE_TYPE(ty, realType)                           \
    case ( type_##ty ):                                                    \
      return (*( realType *)inlineData_m == *( realType *) value.inlineData_m   ); \
      break;
/**
 * Helper macro.
 */
#define COMPARE_EQUALS_PTR_TYPE(ty, realType)                                \
    case ( type_##ty ):                                                      \
      return (* (static_cast<realType *>(ptr_m.pointer)) == *( static_cast<realType *>(value.ptr_m.pointer)) ); \
      break;
/**
 * Helper macro.
 */
#define COMPARE_EQUALS_SEQ_PTR_TYPE(ty, realType)                            \
    case ( type_##ty ): {                                                      \
      if ( (*( realType *)ptr_m.pointer).length() != (*( realType *)value.ptr_m.pointer).length() ) \
          return false;                                                      \
      for (CORBA::ULong i=0; i < (*( realType *)ptr_m.pointer).length(); i++)         \
         if ((*( realType *)value.ptr_m.pointer)[i] != (*( realType *)ptr_m.pointer)[i]) \
	    return false;                                                    \
      return true; \
      break; }

inline bool BACIValue::operator==(const BACIValue &value) const
{
  if (type_m != value.type_m)
    return false;

  switch(type_m)
    {

/// User defined
    case (type_string):
	if (value.ptr_m.pointer==ptr_m.pointer)
	  return true;
	else if (value.ptr_m.pointer==0 || ptr_m.pointer==0)
	  return false;
	else
	  return (ACE_OS::strcmp((const ACE_TCHAR*)ptr_m.pointer, (const ACE_TCHAR*)value.ptr_m.pointer)==0);
	break;

COMPARE_EQUALS_INLINE_TYPE(double, BACIdouble)
COMPARE_EQUALS_INLINE_TYPE(float, BACIfloat)
COMPARE_EQUALS_INLINE_TYPE(long, BACIlong)
COMPARE_EQUALS_INLINE_TYPE(longLong, BACIlongLong)
COMPARE_EQUALS_INLINE_TYPE(uLongLong, BACIuLongLong)
//TOBE deleted COMPARE_EQUALS_INLINE_TYPE(pattern, BACIpattern)

COMPARE_EQUALS_SEQ_PTR_TYPE(floatSeq, BACIfloatSeq)
COMPARE_EQUALS_SEQ_PTR_TYPE(longSeq, BACIlongSeq)
COMPARE_EQUALS_SEQ_PTR_TYPE(stringSeq, BACIstringSeq)

    default:
      return false;
    }

}

#undef COMPARE_EQUALS_PTR_TYPE
#undef COMPARE_EQUALS_INLINE_TYPE

// ------------------------------------------------------------------------
/**
 * Helper macro.
 */
#define COMPARE_LESS_OR_EQUAL_INLINE_TYPE(ty, realType)                    \
    case ( type_##ty ):                                                    \
      return (*( realType *)inlineData_m <= *( realType *)value.inlineData_m); \
      break;
/**
 * Helper macro.
 */
#define COMPARE_LESS_OR_EQUAL_PTR_TYPE(ty, realType)                         \
    case ( type_##ty ):                                                      \
      return (*( realType *)ptr_m.pointer <= *( realType *)value.ptr_m.pointer); \
      break;

inline bool BACIValue::operator<=(const BACIValue &value) const
{
  if (type_m != value.type_m)
    return false;

  switch(type_m)
    {

/// User defined
    case (type_string):
	if (value.ptr_m.pointer==ptr_m.pointer)
	  return true;
	else if (value.ptr_m.pointer==0 || ptr_m.pointer==0)
	  return false;
	else
	  return (ACE_OS::strcmp((const ACE_TCHAR*)ptr_m.pointer, (const ACE_TCHAR*)value.ptr_m.pointer)<=0);
	break;

COMPARE_LESS_OR_EQUAL_INLINE_TYPE(double, BACIdouble)
COMPARE_LESS_OR_EQUAL_INLINE_TYPE(float, BACIfloat)
COMPARE_LESS_OR_EQUAL_INLINE_TYPE(long, BACIlong)
COMPARE_LESS_OR_EQUAL_INLINE_TYPE(longLong, BACIlongLong)
COMPARE_LESS_OR_EQUAL_INLINE_TYPE(uLongLong, BACIuLongLong)
//TOBE deleted COMPARE_LESS_OR_EQUAL_INLINE_TYPE(pattern, BACIpattern)

    default:
      return false;
    }

}

#undef COMPARE_LESS_OR_EQUAL_PTR_TYPE
#undef COMPARE_LESS_OR_EQUAL_INLINE_TYPE

// ------------------------------------------------------------------------
/**
 * Helper macro.
 */
#define COMPARE_LESS_INLINE_TYPE(ty, realType)                            \
    case ( type_##ty ):                                                   \
      return (*( realType *)inlineData_m < *( realType *)value.inlineData_m); \
      break;
/**
 * Helper macro.
 */
#define COMPARE_LESS_PTR_TYPE(ty, realType)                                 \
    case ( type_##ty ):                                                     \
      return (*( realType *)ptr_m.pointer < *( realType *)value.ptr_m.pointer); \
      break;

inline bool BACIValue::operator<(const BACIValue &value) const
{
  if (type_m != value.type_m)
    return false;

  switch(type_m)
    {

/// User defined

    case (type_string):
	if (value.ptr_m.pointer==ptr_m.pointer)
	  return false;
	else if (value.ptr_m.pointer==0 || ptr_m.pointer==0)
	  return false;
	else
	  return (ACE_OS::strcmp((const ACE_TCHAR*)ptr_m.pointer, (const ACE_TCHAR*)value.ptr_m.pointer)<0);
	break;

//COMPARE_LESS_PTR_TYPE(string, BACIstring)
COMPARE_LESS_INLINE_TYPE(double, BACIdouble)
COMPARE_LESS_INLINE_TYPE(float, BACIfloat)
COMPARE_LESS_INLINE_TYPE(long, BACIlong)
COMPARE_LESS_INLINE_TYPE(longLong, BACIlongLong)
COMPARE_LESS_INLINE_TYPE(uLongLong, BACIuLongLong)
//COMPARE_LESS_INLINE_TYPE(pattern, BACIpattern)

    default:
      return false;
    }

}

#undef COMPARE_LESS_PTR_TYPE
#undef COMPARE_LESS_INLINE_TYPE

// ------------------------------------------------------------------------
/**
 * Helper macro.
 */
#define LESS_THAN_DELTA_NUM_INLINE_TYPE(ty, realType)                              \
    case ( type_##ty ): {                                                          \
      realType diff;                                                        \
      if (*(realType*)value.inlineData_m>*( realType *)inlineData_m)            \
      {                                                                         \
      	 diff=(*( realType *)value.inlineData_m - *( realType *)inlineData_m);  \
      }                                                                         \
      else                                                                      \
      {                                                                         \
         diff=(*( realType *)inlineData_m - *( realType *)value.inlineData_m);  \
      }                                                                         \
      return ( delta.ty##Value() > diff);                                       \
      break; }
/**
 * Helper macro.
 */
#define LESS_THAN_DELTA_NUM_SEQ_TYPE(ty, realType, deltaType, realDeltaType)         \
    case ( type_##ty ): {                                                            \
      realDeltaType diff;                                                            \
      for (CORBA::ULong i=0; i < (*( realType *)ptr_m.pointer).length(); i++) {      \
      	 if ((*( realType *)value.ptr_m.pointer)[i]>(*( realType *)ptr_m.pointer)[i])\
      	 {                                                                           \
            diff = (*( realType *)value.ptr_m.pointer)[i]-(*( realType *)ptr_m.pointer)[i]; \
         }                                                                           \
         else                                                                        \
         {                                                                           \
            diff = (*( realType *)ptr_m.pointer)[i]-(*( realType *)value.ptr_m.pointer)[i]; \
         }                                                                           \
         if ((delta.deltaType##Value())<=diff) return false;                         \
      }                                                                              \
      return true;                                                                   \
      break; }

inline bool BACIValue::lessThanDelta(const BACIValue &value, const BACIValue &delta) const
{

  if (type_m != value.type_m)
    return false;


  switch(type_m)
    {
/*TBdeleted
    case (type_pattern) : {

    return (*(BACIpattern*)inlineData_m == *(BACIpattern*)value.inlineData_m);
    break;
    }
*/
/// User defined

LESS_THAN_DELTA_NUM_INLINE_TYPE(double, BACIdouble)
LESS_THAN_DELTA_NUM_INLINE_TYPE(float, BACIfloat)
LESS_THAN_DELTA_NUM_INLINE_TYPE(long, BACIlong)
LESS_THAN_DELTA_NUM_INLINE_TYPE(longLong, BACIlongLong)
LESS_THAN_DELTA_NUM_INLINE_TYPE(uLongLong, BACIuLongLong)
// for sequences delta is scalar
LESS_THAN_DELTA_NUM_SEQ_TYPE(doubleSeq, BACIdoubleSeq, double, BACIdouble)
LESS_THAN_DELTA_NUM_SEQ_TYPE(floatSeq, BACIfloatSeq, float, BACIfloat)
LESS_THAN_DELTA_NUM_SEQ_TYPE(longSeq, BACIlongSeq, long, BACIlong)
/// LESS_THAN_DELTA_NUM_SEQ_TYPE(stringSeq, BACIstringSeq, string, BACIstring)

    default:
      return false;
    }
}

#undef LESS_THAN_DELTA_NUM_INLINE_TYPE

/**
 * Helper macro.
 */
#define LESS_THAN_PERCENT_DELTA_NUM_INLINE_TYPE(ty, realType)                              \
    case ( type_##ty ): {                                                          \
      double diff;                                                        \
      if((*( realType *)inlineData_m) == 0) {                                   \
         if(*( realType *)value.inlineData_m == 0) return true;                \
         else return false;                                                      \
      }                                                                         \
      if (*(realType*)value.inlineData_m>*( realType *)inlineData_m)            \
      {                                                                         \
         diff=((*( realType *)value.inlineData_m - *( realType *)inlineData_m))/(double)(*( realType *)inlineData_m)*100.0;  \
      }                                                                         \
      else                                                                      \
      {                                                                         \
         diff=((*( realType *)inlineData_m - *( realType *)value.inlineData_m))/(double)(*( realType *)inlineData_m)*100.0;  \
      }                                                                         \
      if(diff < 0)                                                              \
         diff = -diff;                                                          \
      return ( (double)delta.ty##Value() > diff);                                       \
      break; }
/**
 * Helper macro.
 */
#define LESS_THAN_PERCENT_DELTA_NUM_SEQ_TYPE(ty, realType, deltaType, realDeltaType)         \
    case ( type_##ty ): {                                                            \
      double diff;                                                            \
      for (CORBA::ULong i=0; i < (*( realType *)ptr_m.pointer).length(); i++) {      \
         if((*( realType *)ptr_m.pointer)[i] == 0) {                                   \
            if((*( realType *)ptr_m.pointer)[i] == 0) continue;                \
            else return false;                                                      \
         }                                                                         \
      	 if ((*( realType *)value.ptr_m.pointer)[i]>(*( realType *)ptr_m.pointer)[i])\
      	 {                                                                           \
            diff = ((*( realType *)value.ptr_m.pointer)[i]-(*( realType *)ptr_m.pointer)[i])/(double)(*( realType *)ptr_m.pointer)[i]*100.0; \
         }                                                                           \
         else                                                                        \
         {                                                                           \
            diff = ((*( realType *)ptr_m.pointer)[i]-(*( realType *)value.ptr_m.pointer)[i])/(double)(*( realType *)ptr_m.pointer)[i]*100.0; \
         }                                                                           \
         if(diff < 0)                                                              \
            diff = -diff;                                                          \
         if ((double)(delta.deltaType##Value())<=diff) return false;                         \
      }                                                                              \
      return true;                                                                   \
      break; }

inline bool BACIValue::lessThanPercentDelta(const BACIValue &value, const BACIValue &delta) const
{

  if (type_m != value.type_m)
    return false;


  switch(type_m)
    {
/*TBdeleted
    case (type_pattern) : {

    return (*(BACIpattern*)inlineData_m == *(BACIpattern*)value.inlineData_m);
    break;
    }
*/
/// User defined

LESS_THAN_PERCENT_DELTA_NUM_INLINE_TYPE(double, BACIdouble)
LESS_THAN_PERCENT_DELTA_NUM_INLINE_TYPE(float, BACIfloat)
LESS_THAN_PERCENT_DELTA_NUM_INLINE_TYPE(long, BACIlong)
LESS_THAN_PERCENT_DELTA_NUM_INLINE_TYPE(longLong, BACIlongLong)
LESS_THAN_PERCENT_DELTA_NUM_INLINE_TYPE(uLongLong, BACIuLongLong)
// for sequences delta is scalar
LESS_THAN_PERCENT_DELTA_NUM_SEQ_TYPE(doubleSeq, BACIdoubleSeq, double, BACIdouble)
LESS_THAN_PERCENT_DELTA_NUM_SEQ_TYPE(floatSeq, BACIfloatSeq, float, BACIfloat)
LESS_THAN_PERCENT_DELTA_NUM_SEQ_TYPE(longSeq, BACIlongSeq, long, BACIlong)
/// LESS_THAN_PERCENT_DELTA_NUM_SEQ_TYPE(stringSeq, BACIstringSeq, string, BACIstring)

    default:
      return false;
    }
}

#undef LESS_THAN_PERCENT_DELTA_NUM_INLINE_TYPE
#undef LESS_THAN_PERCENT_DELTA_NUM_SEQ_TYPE

// ------------------------------------------------------------------------
/**
 * Helper macro.
 */
#define NO_DELTA(ty, zero)                              \
    case ( type_##ty ): {                               \
      return ( ty##Value() == zero);                  \
      break; }

inline bool BACIValue::noDelta() const
{

  switch(type_m)
    {

/// User defined
//TOBE deleted NO_DELTA(pattern, 0)
NO_DELTA(double, 0.0)
NO_DELTA(float, 0.0)
NO_DELTA(long, 0)
NO_DELTA(longLong, 0)
NO_DELTA(uLongLong, 0)
    default:
      return true;			// everything is 'no delta', because there is no delta
    }
}

#undef NO_DELTA

// ----------------------------[ Accessors ]-------------------------------
/**
 * Helper macro.
 */
#define ACCESSOR_INLINE_TYPE(ty, realType)     \
inline realType BACIValue::ty##Value() const \
{                                              \
  if (type_m != type_##ty )                      \
    return 0;                                  \
  else                                         \
    return *(( realType *)inlineData_m);         \
}

/**
 * Helper macro.
 */
#define ACCESSOR_PTR_TYPE(ty, realType)        \
inline realType BACIValue::ty##Value() const \
{                                              \
  if (type_m != type_##ty )                      \
    return 0;                                  \
  else                                         \
    return *(( realType *)ptr_m.pointer);        \
}
/// User defined

inline char* BACIValue::getValue(const char ** v) const
{
    ACE_UNUSED_ARG(v);
    return const_cast<char*>(stringValue());
}

inline char* BACIValue::getValue(char ** v) const
{
    ACE_UNUSED_ARG(v);
    return const_cast<char*>(stringValue());
}

inline const ACE_TCHAR* BACIValue::stringValue() const
{
  if (type_m != type_string)
    return 0;
  else
    return (const ACE_TCHAR*)ptr_m.pointer;
}

ACCESSOR_INLINE_TYPE(double, BACIdouble)
ACCESSOR_INLINE_TYPE(float, BACIfloat)
ACCESSOR_INLINE_TYPE(long, BACIlong)
ACCESSOR_INLINE_TYPE(longLong, BACIlongLong)
ACCESSOR_INLINE_TYPE(uLongLong, BACIuLongLong)
//ACCESSOR_INLINE_TYPE(pattern, BACIpattern)


ACCESSOR_PTR_TYPE(doubleSeq, BACIdoubleSeq)
ACCESSOR_PTR_TYPE(floatSeq, BACIfloatSeq)
ACCESSOR_PTR_TYPE(longSeq, BACIlongSeq)
ACCESSOR_PTR_TYPE(stringSeq, BACIstringSeq)

#undef ACCESSOR_PTR_TYPE
#undef ACCESSOR_INLINE_TYPE

inline void * BACIValue::pointerValue() const
{
  if (type_m != type_pointer)
    return 0;
  else
    return ptr_m.pointer;
}

// ----------------------------[ Mutators ]--------------------------------
/**
 * Helper macro for method implementation.
 */
#define MUTATOR_INLINE_TYPE(ty, realType)                 \
inline bool BACIValue::ty##Value(const realType &value) \
{                                                         \
  if (type_m != type_##ty )                                 \
    if (!setType(type_##ty))                              \
       return false;                                      \
                                                          \
  *(( realType *)inlineData_m) = value;                     \
  return true;                                            \
}

/**
 * Helper macro for method implementation.
 */
#define MUTATOR_PTR_TYPE(ty, realType)                    \
inline bool BACIValue::ty##Value(const realType &value) \
{                                                         \
  if (type_m != type_##ty )                                 \
    if (!setType(type_##ty))                              \
       return false;                                      \
                                                          \
  *(( realType *)ptr_m.pointer) = value;                    \
  return true;                                            \
}


inline bool BACIValue::setValue(const char *value)
{
    return stringValue(value);
}

/// User defined
inline bool BACIValue::stringValue(const char *value)
{
  if (type_m != type_string)
    if (!setType(type_string))
	return false;

	if (value==0)
	  ptr_m.pointer = 0;
	else
	{
	  if (ptr_m.pointer) delete[] (ACE_TCHAR*)ptr_m.pointer;
	  ACE_TCHAR * str = new ACE_TCHAR[ACE_OS::strlen(value)+1];
	  ACE_OS::strcpy(str, value);
	  ptr_m.pointer = str;
	}
	return true;
}

MUTATOR_INLINE_TYPE(double, BACIdouble)
MUTATOR_INLINE_TYPE(float, BACIfloat)
MUTATOR_INLINE_TYPE(long, BACIlong)
MUTATOR_INLINE_TYPE(longLong, BACIlongLong)
MUTATOR_INLINE_TYPE(uLongLong, BACIuLongLong)
//TOBE deleted MUTATOR_INLINE_TYPE(pattern, BACIpattern)

MUTATOR_PTR_TYPE(doubleSeq, BACIdoubleSeq)
MUTATOR_PTR_TYPE(floatSeq, BACIfloatSeq)
MUTATOR_PTR_TYPE(longSeq, BACIlongSeq)
MUTATOR_PTR_TYPE(stringSeq, BACIstringSeq)

#undef MUTATOR_PTR_TYPE
#undef MUTATOR_INLINE_TYPE

inline bool BACIValue::pointerValue(void * value)
{
  if (type_m != type_pointer)
    if (!setType(type_pointer))
       return false;

  ptr_m.pointer = value;
  return true;
}

/*___oOo___*/





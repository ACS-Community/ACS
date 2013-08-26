#ifndef __cdb__Field_h__
#define __cdb__Field_h__
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2011
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
* "@(#) $Id: cdbField.h,v 1.31 2011/10/28 15:05:05 hsommer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2011-10-28  created
*/

#include <iostream>

#include "acsutil.h"

#include "cdbData_Types.h"

#include "cdbExport.h"

// ------------------[ Stream extraction/insertion ]-----------------------

#if !defined(__GNUG__)

//{secret}
std::ostream& operator<<(std::ostream &os, cdb::LongLong ll);

//{secret}
std::istream& operator>>(std::istream &is, cdb::LongLong &ll);

//{secret}
std::ostream& operator<<(std::ostream &os, cdb::ULongLong ll);

//{secret}
std::istream& operator>>(std::istream &is, cdb::ULongLong &ll);

#endif //!defined(__GNUG__)

#if defined(CDB_HAS_ANY)

//
// DESCRIPTION: Stream insertion of the Any type.
//
// Stream extraction/insertion operators for the Any type. Any's binary
// image is stringified as text containing hexadecimal octets and written
// to the stream as a single word.
//
// EXAMPLE:
//
//      Any a;
//      // .. do something with a
//      cout << a << endl;
//
#ifndef MAKE_VXWORKS
std::ostream& operator<<(std::ostream &os, const cdb::Any& any);
#else
std::ostream& operator<<(std::ostream &os, const cdb::Any& any);
#endif

//{secret}
#ifndef MAKE_VXWORKS
std::istream& operator>>(std::istream &is, cdb::Any &any);
#else
std::istream& operator>>(std::istream &is, cdb::Any &any);
#endif

#endif // defined(CDB_HAS_ANY)

namespace cdb {

// ------------------------[ Null field types ]----------------------------

//{partOf:Field::IsNull}
#define CDB_FIELD_NULL          1

//{partOf:Field::IsNull}
#define CDB_FIELD_UNINITIALIZED 2

//{partOf:Field::IsNull}
#define CDB_FIELD_NONEXISTENT   3

// ------------------------------[ Field ]---------------------------------

//
// DESCRIPTION: A field in the database.
//
// A field is the basic building block of a database. It stores one value,
// and knows its type so that it can be correctly interpreted.
//
class cdb_EXPORT Field
{
public:  
  //{group:Type manipulation}
  // DESCRIPTION: Enumeration of all supported data types.
  //
  enum Type
  {
    tyNull=0,     // No-value. Can also be used to imply special meaning.

    tyAny=1,      // The CORBA::Any type.
    tyString=2,   // A string.
    tyOctet=3,    // 8-bit unsigned integer.
    tyBoolean=4,  // A boolean value (true/false).

    tyShort=5,    // 16-bit signed integer.
    tyLong=6,     // 32-bit signed integer.
    tyLongLong=7, // 64-bit signed integer.

    tyUShort=8,   // 16-bit unsigned integer.
    tyULong=9,    // 32-bit unsigned integer.
    tyULongLong=10,// 64-bit unsigned integer.

    tyFloat=11,    // IEEE 4-byte floating point number.
    tyDouble=12,   // IEEE 8-byte floating point number.

    tyArray=13,    // Abstract superclass of all arrays.

    tyAnyArray=14,
    tyStringArray=15,
    tyOctetArray=16,

    tyShortArray=17,
    tyLongArray=18,
    tyLongLongArray=19,

    tyUShortArray=20,
    tyULongArray=21,
    tyULongLongArray=22,

    tyFloatArray=23,
    tyDoubleArray=24
  };

  // ----------------------------------------------------------------------
  // GROUP = Static instances
  // ----------------------------------------------------------------------

  //
  // DESCRIPTION: A global predefined Null field.
  //
  // When calling Field::Null.IsNull(), a value of CDB_FIELD_NULL is
  // returned.
  //
  static const Field Null;

  //
  // DESCRIPTION: A global predefined field denoting a non-existent field.
  //
  // When calling Field::Nonexistent.IsNull(), a value of
  // CDB_FIELD_NONEXISTENT is returned
  //
  static const Field Nonexistent;

  //
  // DESCRIPTION: Construct a field.
  //
  // PARAMETERS:
  //   ty      - Data-type that this field will store.
  //   nBound  - For compsite types (strings, arrays), specifies up to how
  //             many elements may be contained in the field.
  //
  Field(Type ty, ULong nBound = 0);

  //
  // DESCRIPTION: Construct a field. The field will carry a null value.
  //
  Field();

  //
  // DESCRIPTION: Copy constructor: construct a field that will store the
  //              same data as the specified field.
  //
  Field(const Field &fld);

#if defined(CDB_HAS_ANY)
  Field(const Any &);
#endif // defined(CDB_HAS_ANY)

  Field(const String &);
  Field(Octet);

  Field(Short);
  Field(Long);
  Field(LongLong);

  Field(UShort);
  Field(ULong);
  Field(ULongLong);

  Field(Float);
  Field(Double);

#if defined(CDB_HAS_ANY)
  Field(const AnyArray &);
#endif // defined(CDB_HAS_ANY)
  Field(const StringArray &);
  Field(const OctetArray &);

  Field(const ShortArray &);
  Field(const LongArray &);
  Field(const LongLongArray &);

  Field(const UShortArray &);
  Field(const ULongArray &);
  Field(const ULongLongArray &);

  Field(const FloatArray &);
  Field(const DoubleArray &);

  Field &operator=(const Field &);

  ~Field();

  // ----------------------------------------------------------------------
  // GROUP = Type manipulation
  // ----------------------------------------------------------------------

  //
  // DESCRIPTION: Set the data type that this field will store.
  //
  // Makes the field capable of storing data of type ty. If the type
  // currently being stored by the filed does not match ty, it is properly
  // disposed. After using this function, the user should not assume that
  // the data has been initialized, i.e. using Get before a Set is illegal.
  //
  // EXAMPLE:
  //
  //       Field fld;
  //       fld.SetType(Field::tyString);
  //
  Boolean SetType(Type ty, ULong ulBound = 0); 
  Type GetType() const { return m_ty; }
  ULong GetBound() const { return ptr.m_ulBound; };
  
  //
  // DESCRIPTION: Returns the reason why the field was set to 0.
  //
  // RETURN VALUE:

  //   A code explaining the reason why the field is set to Null. If 0 is
  //   returned, the field is not Null. Otherwise, one of these can be
  //   expected:
  //
  //    CDB_FIELD_NULL - The field is Null because it is meant to carry
  //                     a Null value.
  //
  //    CDB_FIELD_UNINITIALIZED - The field is Null because it has not
  //                              yet been initialized to another value.
  //
  //    CDB_FIELD_NONEXISTENT - The field does not exist.
  //
  ULong IsNull() const { return (m_ty == tyNull)?m_ulWhyNull:0; }

  // ----------------------------------------------------------------------
  // GROUP = Accessors
  // ----------------------------------------------------------------------
  
  //
  //
  //
  Boolean GetOctet(Octet &) const;
  //{partOf:GetOctet}
  Boolean GetString(String &) const;
  Boolean getValue(String &) const;
  //{partOf:GetOctet}
  Boolean GetBoolean(Boolean &) const;

  //{partOf:GetOctet}
  Boolean GetShort(Short &) const;
  //{partOf:GetOctet}
  Boolean GetLong(Long &) const;
  Boolean getValue(Long &) const;
  //{partOf:GetOctet}
  Boolean GetLongLong(LongLong &) const;
  Boolean getValue(LongLong & val) const { return GetLongLong(val); }
    
  //{partOf:GetOctet}
  Boolean GetUShort(UShort &) const;
  //{partOf:GetOctet}
  Boolean GetULong(ULong &) const;
  Boolean getValue(ULong &) const;
  //{partOf:GetOctet}
  Boolean GetULongLong(ULongLong &) const;
  Boolean getValue(ULongLong &val) const {  return GetULongLong(val); }

  //{partOf:GetOctet}
  Boolean GetFloat(Float &) const;
  Boolean getValue(Float &) const;
  //{partOf:GetOctet}
  Boolean GetDouble(Double &) const;
  Boolean getValue(Double &) const;

  //{partOf:GetOctet}
  Boolean GetStringArray(StringArray &) const;
  //{partOf:GetOctet}
  Boolean GetOctetArray(OctetArray &) const;

  //{partOf:GetOctet}
  Boolean GetShortArray(ShortArray &) const;
  //{partOf:GetOctet}
  Boolean GetLongArray(LongArray &) const;
  //{partOf:GetOctet}
  Boolean GetLongLongArray(LongLongArray &) const;

  //{partOf:GetOctet}
  Boolean GetUShortArray(UShortArray &) const;
  //{partOf:GetOctet}
  Boolean GetULongArray(ULongArray &) const;
  //{partOf:GetOctet}
  Boolean GetULongLongArray(ULongLongArray &) const;

  //{partOf:GetOctet}
  Boolean GetFloatArray(FloatArray &) const;
  //{partOf:GetOctet}
  Boolean GetDoubleArray(DoubleArray &) const;
  Boolean getValue(DoubleArray &) const;

  StringArray * GetStringArray();
  LongArray * GetLongArray();

  Field operator[](ULong idx) const; 

#if defined(CDB_HAS_ANY)
  //{partOf:GetOctet}
  Boolean GetAny(Any &) const;
  //{partOf:GetOctet}
  Boolean GetAnyArray(AnyArray &) const;
#endif // defined(CDB_HAS_ANY)

  // ----------------------------------------------------------------------
  // GROUP = Mutators
  // ----------------------------------------------------------------------

  //
  // If the field currently contains other type of data than the one being
  // stored, the operation has no effect and false is returned.
  //
  Boolean SetString(const String &);
  Boolean SetOctet(Octet);
  Boolean SetBoolean(Boolean);

  Boolean SetShort(Short);
  Boolean SetLong(Long);
  Boolean SetLongLong(LongLong);

  Boolean SetUShort(UShort);
  Boolean SetULong(ULong);
  Boolean SetULongLong(ULongLong);

  Boolean SetFloat(Float);
  Boolean SetDouble(Double);

  Boolean SetStringArray(const StringArray &);
  Boolean SetOctetArray(const OctetArray &);

  Boolean SetShortArray(const ShortArray &);
  Boolean SetLongArray(const LongArray &);
  Boolean SetLongLongArray(const LongLongArray &);

  Boolean SetUShortArray(const UShortArray &);
  Boolean SetULongArray(const ULongArray &);
  Boolean SetULongLongArray(const ULongLongArray &);

  Boolean SetFloatArray(const FloatArray &);
  Boolean SetDoubleArray(const DoubleArray &);

#if defined(CDB_HAS_ANY)
  Boolean SetAny(const Any &);
  Boolean SetAnyArray(const AnyArray &);
#endif // defined(CDB_HAS_ANY)

  // ----------------------------------------------------------------------
  // GROUP = Conversion helpers
  // ----------------------------------------------------------------------

  Boolean ToString(String &, Boolean bType = 1) const;
  Boolean FromString(const String &);

#if defined(CDB_HAS_ANY)
  //Boolean ToAny(Any &) const;
  //Boolean ToAny(Any &) const;
  //Boolean FromAny(const Any &);
#endif // defined(CDB_HAS_ANY)

protected:
  //
  // Identifies the type of data currently being stored.
  //
  Type  m_ty;  
  union
  {
    //
    // Inlined data can be up to 8 bytes in length. This is sufficient for
    // storing octets, short, long, and longlong integers, doubles and
    // floats. Other types store a pointer to the actual data and a bound
    // on the amount of data that can be stored there (e.g., number of
    // characters for strings). A bound of 0 indicates "no limits".
    //
    Octet  m_inline[8];
    struct
    {
      ULong  m_ulBound;
      void  *m_pointer;
    } ptr;
    
    ULong m_ulWhyNull;
  };

  Boolean Convert( Octet& val ) const;
  Boolean Convert( Short& val ) const;
  Boolean Convert( Long& val ) const;
  Boolean Convert( LongLong& val ) const;
  Boolean Convert( UShort& val ) const;
  Boolean Convert( ULong& val ) const;
  Boolean Convert( ULongLong& val ) const;
  Boolean Convert( Float& val ) const;
  Boolean Convert( Double& val ) const;

  Boolean Convert(StringArray &val) const;
  Boolean Convert(LongArray &val) const;
  Boolean Convert(DoubleArray &val) const;
};

//
// DESCRIPTION: An array of fields.
//
typedef std::vector<Field> FieldArray;

//#include "cdbField.i"

 }; 

#endif // __cdb__Field_h__

// ************************************************************************
//
// REVISION HISTORY:
//
//   $Log: cdbField.h,v $
//   Revision 1.31  2011/10/28 15:05:05  hsommer
//   Manually fixed "no LGPL license text" issue reported by addCopyright.py
//
//   Revision 1.30  2006/09/01 02:20:54  cparedes
//   small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
//
//   Revision 1.29  2005/08/23 15:22:54  vwang
//   to add float into BACI
//
//   Revision 1.28  2005/02/14 10:39:49  acaproni
//   Some changes to reduce the coding standards number of errors
//
//   Revision 1.27  2003/07/10 15:23:16  bjeram
//   support for longlong and uLongLong
//
//   Revision 1.26  2003/07/09 08:07:35  bjeram
//   ported to gcc 3.2
//
//   Revision 1.25  2003/05/06 13:32:01  bjeram
//   port to Tornado 2.2
//
//   Revision 1.24  2003/01/28 16:43:50  vltsccm
//   gchiozzi: patch for cdb module to create lib/endorsed directory, since CVS cannot restore empty directories
//
//   Revision 1.23  2003/01/24 10:44:04  vltsccm
//   cdb1.23
//
//   Revision 1.22  2003/01/20 15:12:19  vltsccm
//   cdb1.22
//
//   Revision 1.21  2003/01/20 10:45:53  vltsccm
//   cdb1.21
//
//   Revision 1.20  2002/12/05 16:03:58  vltsccm
//   cdb1.20
//
//   Revision 1.19  2002/11/25 16:04:50  vltsccm
//   cdb1.19
//
//   Revision 1.18  2002/11/13 14:53:04  vltsccm
//   cdb1.18
//
//   Revision 1.17  2002/11/13 10:22:31  vltsccm
//   cdb1.17
//
//   Revision 1.16  2002/11/06 08:37:04  vltsccm
//   cdb1.16
//
//   Revision 1.15.1.23  2002/11/05 16:05:13  vltsccm
//   cdb1.15.1.23
//
//   Revision 1.15.1.22  2002/11/05 13:46:31  vltsccm
//   cdb1.15.1.22
//
//   Revision 1.15.1.21  2002/11/05 10:41:14  vltsccm
//   cdb1.15.1.21
//
//   Revision 1.15.1.20  2002/11/01 12:49:03  vltsccm
//   cdb1.15.1.20
//
//   Revision 1.15.1.19  2002/10/30 07:56:44  vltsccm
//   cdb1.15.1.19
//
//   Revision 1.15.1.18  2002/10/25 12:44:24  vltsccm
//   cdb1.15.1.18
//
//   Revision 1.15.1.17  2002/10/24 13:08:44  vltsccm
//   cdb1.15.1.17
//
//   Revision 1.15.1.16  2002/10/16 11:43:45  vltsccm
//   cdb1.15.1.16
//
//   Revision 1.15.1.15  2002/10/14 22:26:10  vltsccm
//   cdb1.15.1.15
//
//   Revision 1.15.1.14  2002/10/14 12:18:32  vltsccm
//   cdb1.15.1.14
//
//   Revision 1.15.1.13  2002/10/04 16:20:23  vltsccm
//   cdb1.15.1.13
//
//   Revision 1.15.1.12  2002/10/02 12:54:14  vltsccm
//   cdb1.15.1.12
//
//   Revision 1.15.1.11  2002/10/01 10:33:25  vltsccm
//   cdb1.15.1.11
//
//   Revision 1.15.1.10  2002/09/30 13:56:52  vltsccm
//   cdb1.15.1.10
//
//   Revision 1.15.1.9  2002/09/26 14:13:10  vltsccm
//   cdb1.15.1.9
//
//   Revision 1.15.1.8  2002/09/26 07:45:46  vltsccm
//   cdb1.15.1.8
//
//   Revision 1.15.1.7  2002/09/17 16:19:22  vltsccm
//   cdb1.15.1.7
//
//   Revision 1.15.1.6  2002/09/17 11:15:47  vltsccm
//   cdb1.15.1.6
//
//   Revision 1.15.1.5  2002/09/02 09:37:06  vltsccm
//   cdb1.15.1.5
//
//   Revision 1.15.1.4  2002/08/09 09:35:23  vltsccm
//   cdb1.15.1.4
//
//   Revision 1.15.1.3  2002/07/24 07:29:11  vltsccm
//   cdb1.15.1.3
//
//   Revision 1.15.1.2  2002/07/12 09:58:17  vltsccm
//   cdb1.15.1.2
//
//   Revision 1.15+.1.1  2002/07/09 09:40:09  vltsccm
//   cdb1.15.1
//
//   Revision 1.15  2002/02/05 17:50:08  vltsccm
//   cdb1.15
//
//   Revision 1.14  2002/01/14 21:14:18  vltsccm
//   cdb1.14
//
//   Revision 1.13  2001/10/19 09:56:22  vltsccm
//   cdb1.13
//
//   Revision 1.12  2001/09/18 10:07:12  vltsccm
//   cdb1.12
//
//   Revision 1.11  2001/07/12 07:48:27  vltsccm
//   cdb1.11
//
//   Revision 1.10  2001/07/11 09:16:16  vltsccm
//   cdb1.10
//
//   Revision 1.6  2000/12/07 18:00:41  vltsccm
//   cdb1.6
//
//   Revision 1.5  2000/11/17 13:14:58  vltsccm
//   cdb1.5
//
//   Revision 1.4  2000/10/20 13:51:28  vltsccm
//   cdb1.4
//
//   Revision 1.3  2000/10/20 13:51:27  vltsccm
//   cdb1.3
//
//   Revision 1.2  2000/10/20 13:51:26  vltsccm
//   cdb1.2
//
//   Revision 1.1  2000/10/20 13:51:26  vltsccm
//   cdb1.1
//
//   Revision 1.0  2000/10/20 13:51:26  vltsccm
//   cdb1.0
//
//   Revision 1.3  2000/10/13 16:03:02  vltsccm
//   cdb1.3
//
//   Revision 1.2  2000/09/13 14:49:28  vltsccm
//   cdb1.2
//
//   Revision 1.1  2000/09/06 15:42:11  vltsccm
//   cdb1.1
//
//   Revision 1.1  2000/06/13 07:26:24  kzagar
//   CDB, initial commit. Documentation not yet finished.
//
// ************************************************************************

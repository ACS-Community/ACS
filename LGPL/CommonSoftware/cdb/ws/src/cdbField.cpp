/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
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
* "@(#) $Id: cdbField.cpp,v 1.32 2008/07/25 07:24:24 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2003-07-10 fixed GetLongLong and GetULongLong
* bjeram 2002-10-25 added Convert to GetLong
* almamgr 2002-02-05 Changed #include <> with #include "" for local files.
* almamgr 2002-02-05 Moved here BIG inline methods. Problems with purify.
*/

#include <vltPort.h>

#include "cdbField.h"

#if defined(CDB_HAS_ANY)
#  include <ace/OS.h>
#  define strcasecmp ACE_OS::strcasecmp
#else
#  define strcasecmp strcasecmp
#endif

#include <sstream>
#include <string>

using namespace cdb;
namespace cdb {

// -----------------[ Set type, assignment operator ]----------------------

Boolean Field::SetType(Type ty, ULong ulBound)
{
  if(m_ty != tyNull)
    {
      switch(m_ty)
        {
#if defined(CDB_HAS_ANY)
        case tyAny:
          delete (Any*)ptr.m_pointer;
          break;
        case tyAnyArray:
          delete (AnyArray*)ptr.m_pointer;
          break;
#endif // defined(CDB_HAS_ANY)
        case tyString:
          delete (String*)ptr.m_pointer;
          break;
        case tyStringArray:
          delete (StringArray*)ptr.m_pointer;
          break;
        case tyOctetArray:
          delete (OctetArray*)ptr.m_pointer;
          break;
        case tyShortArray:
          delete (ShortArray*)ptr.m_pointer;
          break;
        case tyLongArray:
          delete (LongArray*)ptr.m_pointer;
          break;
        case tyLongLongArray:
          delete (LongLongArray*)ptr.m_pointer;
          break;
        case tyUShortArray:
          delete (UShortArray*)ptr.m_pointer;
          break;
        case tyULongArray:
          delete (ULongArray*)ptr.m_pointer;
          break;
        case tyULongLongArray:
          delete (ULongLongArray*)ptr.m_pointer;
          break;
        case tyFloatArray:
          delete (FloatArray*)ptr.m_pointer;
          break;
        case tyDoubleArray:
          delete (DoubleArray*)ptr.m_pointer;
          break;
        default:
          break;
        }
    }
  
  m_ty = ty;
  if(ty == tyNull)
    {
      m_ulWhyNull = ulBound;
      return TRUE;
    }

  ptr.m_ulBound = ulBound;
  switch(m_ty)
    {
    case tyNull:
#if defined(_DEBUG)
      memset(m_inline, 0xCD, sizeof(m_inline));
#else
      memset(m_inline, 0x00, sizeof(m_inline));
#endif
      return TRUE;
#if defined(CDB_HAS_ANY)
    case tyAny:
      if((ptr.m_pointer = new Any) == 0) {
      	return FALSE;
      }
      break;
#endif // defined(CDB_HAS_ANY)
    case tyString:
      if((ptr.m_pointer = new String) == 0) {
      	return FALSE;
      }
      break;
    case tyOctet:
      new(m_inline) Octet;
      break;
    case tyBoolean:
      new(m_inline) Boolean;
      break;
    case tyShort:
      new(m_inline) Short;
      break;
    case tyLong:
      new(m_inline) Long;
      break;
    case tyLongLong:
      new(m_inline) LongLong;
      break;
    case tyUShort:
      new(m_inline) UShort;
      break;
    case tyULong:
      new(m_inline) ULong;
      break;
    case tyULongLong:
      new(m_inline) ULongLong;
      break;
    case tyFloat:
      new(m_inline) Float;
      break;
    case tyDouble:
      new(m_inline) Double;
      break;
#if defined(CDB_HAS_ANY)
    case tyAnyArray:
      if((ptr.m_pointer = new AnyArray) == 0) {
      	return FALSE;
      }
      break;
#endif // defined(CDB_HAS_ANY)
    case tyStringArray:
      if((ptr.m_pointer = new StringArray) == 0) {
      	return FALSE;
      }
      break;
    case tyOctetArray:
      if((ptr.m_pointer = new OctetArray) == 0) {
      	return FALSE;
      }
      break;
    case tyShortArray:
      if((ptr.m_pointer = new ShortArray) == 0) {
      	return FALSE;
      }
      break;
    case tyLongArray:
      if((ptr.m_pointer = new LongArray) == 0) {
      	return FALSE;
      }
      break;
    case tyLongLongArray:
      if((ptr.m_pointer = new LongLongArray) == 0) {
      	return FALSE;
      }
      break;
    case tyUShortArray:
      if((ptr.m_pointer = new UShortArray) == 0) {
      	return FALSE;
      }
      break;
    case tyULongArray:
      if((ptr.m_pointer = new ULongArray) == 0) {
      	return FALSE;
      }
      break;
    case tyULongLongArray:
      if((ptr.m_pointer = new ULongLongArray) == 0) {
      	return FALSE;
      }
      break;
    case tyFloatArray:
      if((ptr.m_pointer = new FloatArray) == 0) {
      	return FALSE;
      }
      break;
    case tyDoubleArray:
      if((ptr.m_pointer = new DoubleArray) == 0) {
      	return FALSE;
      }
      break;
    default:
      return FALSE;
    }

  return TRUE;
}

Field& Field::operator=(const Field& fld)
{
  SetType(fld.m_ty, fld.ptr.m_ulBound);

  switch(fld.m_ty)
    {
#if defined(CDB_HAS_ANY)
    case tyAny:
      *(Any*)ptr.m_pointer = *(Any*)fld.ptr.m_pointer;
      break;
#endif // defined(CDB_HAS_ANY)
    case tyString:
      *(String*)ptr.m_pointer = *(String*)fld.ptr.m_pointer;
      break;
    case tyOctet:
      new(m_inline) Octet(*(Octet*)fld.m_inline);
      break;
    case tyBoolean:
      new(m_inline) Boolean;
      break;
    case tyShort:
      new(m_inline) Short(*(Short*)fld.m_inline);
      break;
    case tyLong:
      new(m_inline) Long(*(Long*)fld.m_inline);
      break;
    case tyLongLong:
      new(m_inline) LongLong(*(LongLong*)fld.m_inline);
      break;
    case tyUShort:
      new(m_inline) UShort(*(UShort*)fld.m_inline);
      break;
    case tyULong:
      new(m_inline) ULong(*(ULong*)fld.m_inline);
      break;
    case tyULongLong:
      new(m_inline) ULongLong(*(ULongLong*)fld.m_inline);
      break;
    case tyFloat:
      new(m_inline) Float(*(Float*)fld.m_inline);
      break;
    case tyDouble:
      new(m_inline) Double(*(Double*)fld.m_inline);
      break;
#if defined(CDB_HAS_ANY)
    case tyAnyArray:
      *(AnyArray*)ptr.m_pointer = *(AnyArray*)fld.ptr.m_pointer;
      break;
#endif // defined(CDB_HAS_ANY)
    case tyStringArray:
      *(StringArray*)ptr.m_pointer = *(StringArray*)fld.ptr.m_pointer;
      break;
    case tyOctetArray:
      *(OctetArray*)ptr.m_pointer = *(OctetArray*)fld.ptr.m_pointer;
      break;
    case tyShortArray:
      *(ShortArray*)ptr.m_pointer = *(ShortArray*)fld.ptr.m_pointer;
      break;
    case tyLongArray:
      *(LongArray*)ptr.m_pointer = *(LongArray*)fld.ptr.m_pointer;
      break;
    case tyLongLongArray:
      *(LongLongArray*)ptr.m_pointer = *(LongLongArray*)fld.ptr.m_pointer;
      break;
    case tyUShortArray:
      *(UShortArray*)ptr.m_pointer = *(UShortArray*)fld.ptr.m_pointer;
      break;
    case tyULongArray:
      *(ULongArray*)ptr.m_pointer = *(ULongArray*)fld.ptr.m_pointer;
      break;
    case tyULongLongArray:
      *(ULongLongArray*)ptr.m_pointer = *(ULongLongArray*)fld.ptr.m_pointer;
      break;
    case tyFloatArray:
      *(FloatArray*)ptr.m_pointer = *(FloatArray*)fld.ptr.m_pointer;
      break;
    case tyDoubleArray:
      *(DoubleArray*)ptr.m_pointer = *(DoubleArray*)fld.ptr.m_pointer;
      break;
    default:
      break;
    }

  return *this;
}

// ----------------------------[ Accessors ]-------------------------------

#if defined(CDB_HAS_ANY)
Boolean Field::GetAny(Any &val) const
{
  switch(m_ty)
    {
    case tyAny:
      val = *((Any*)ptr.m_pointer);
      return TRUE;
    case tyString:
      val <<= ((String*)ptr.m_pointer)->c_str();
      return TRUE;
    case tyOctet:
      val <<= Any::from_octet(*((Octet*)m_inline));
      return TRUE;
    case tyBoolean:
      val <<= Any::from_boolean(*((Boolean*)m_inline));
      return TRUE;
    case tyShort:
      val <<= *((Short*)m_inline);
      return TRUE;
    case tyLong:
      val <<= *((Long*)m_inline);
      return TRUE;
    case tyLongLong:
      val <<= *((LongLong*)m_inline);
      return TRUE;
    case tyUShort:
      val <<= *((UShort*)m_inline);
      return TRUE;
    case tyULong:
      val <<= *((ULong*)m_inline);
      return TRUE;
    case tyULongLong:
      val <<= *((ULongLong*)m_inline);
      return TRUE;
    case tyFloat:
      val <<= *((Float*)m_inline);
      return TRUE;
    case tyDouble:
      val <<= *((Double*)m_inline);
      return TRUE;
    case tyStringArray:
    {
      Any any;
      Anys anys;
      anys.length((*(StringArray*)ptr.m_pointer).size());
      int n = 0; StringArray::iterator iter = (*(StringArray*)ptr.m_pointer).begin();
      for (; iter != (*(StringArray*)ptr.m_pointer).begin();
	   iter++)
	  { any <<= iter->c_str(); anys[n++] = any; }
      val <<= anys;
      return TRUE;
      break;
    }
    case tyOctetArray:
    {
      Any any;
      Anys anys;
      anys.length((*(OctetArray*)ptr.m_pointer).size());
      int n = 0; OctetArray::iterator iter = (*(OctetArray*)ptr.m_pointer).begin();
      for (; iter != (*(OctetArray*)ptr.m_pointer).begin();
	   iter++)
	  { any <<= Any::from_octet(*iter); anys[n++] = any; }
      val <<= anys;
      return TRUE;
      break;
    }
    case tyShortArray:
    {
      Any any;
      Anys anys;
      anys.length((*(ShortArray*)ptr.m_pointer).size());
      int n = 0; ShortArray::iterator iter = (*(ShortArray*)ptr.m_pointer).begin();
      for (; iter != (*(ShortArray*)ptr.m_pointer).begin();
	   iter++)
	  { any <<= (*iter); anys[n++] = any; }
      val <<= anys;
      return TRUE;
      break;
    }
    case tyLongArray:
    {
      Any any;
      Anys anys;
      anys.length((*(LongArray*)ptr.m_pointer).size());
      int n = 0; LongArray::iterator iter = (*(LongArray*)ptr.m_pointer).begin();
      for (; iter != (*(LongArray*)ptr.m_pointer).begin();
	   iter++)
	  { any <<= (*iter); anys[n++] = any; }
      val <<= anys;
      return TRUE;
      break;
    }
    case tyLongLongArray:
    {
      Any any;
      Anys anys;
      anys.length((*(LongLongArray*)ptr.m_pointer).size());
      int n = 0; LongLongArray::iterator iter = (*(LongLongArray*)ptr.m_pointer).begin();
      for (; iter != (*(LongLongArray*)ptr.m_pointer).begin();
	   iter++)
	  { any <<= (*iter); anys[n++] = any; }
      val <<= anys;
      return TRUE;
      break;
    }
    case tyUShortArray:
    {
      Any any;
      Anys anys;
      anys.length((*(UShortArray*)ptr.m_pointer).size());
      int n = 0; UShortArray::iterator iter = (*(UShortArray*)ptr.m_pointer).begin();
      for (; iter != (*(UShortArray*)ptr.m_pointer).begin();
	   iter++)
	  { any <<= (*iter); anys[n++] = any; }
      val <<= anys;
      return TRUE;
      break;
    }
    case tyULongArray:
    {
      Any any;
      Anys anys;
      anys.length((*(ULongArray*)ptr.m_pointer).size());
      int n = 0; ULongArray::iterator iter = (*(ULongArray*)ptr.m_pointer).begin();
      for (; iter != (*(ULongArray*)ptr.m_pointer).begin();
	   iter++)
	  { any <<= (*iter); anys[n++] = any; }
      val <<= anys;
      return TRUE;
      break;
    }
    case tyULongLongArray:
    {
      Any any;
      Anys anys;
      anys.length((*(ULongLongArray*)ptr.m_pointer).size());
      int n = 0; ULongLongArray::iterator iter = (*(ULongLongArray*)ptr.m_pointer).begin();
      for (; iter != (*(ULongLongArray*)ptr.m_pointer).begin(); iter++)
      {
	  	any <<= (*iter); 
	  	anys[n++] = any; 
	  }
      val <<= anys;
      return TRUE;
      break;
    }
    case tyFloatArray:
    {
      Any any;
      Anys anys;
      anys.length((*(FloatArray*)ptr.m_pointer).size());
      int n = 0; FloatArray::iterator iter = (*(FloatArray*)ptr.m_pointer).begin();
      for (; iter != (*(FloatArray*)ptr.m_pointer).begin(); iter++)
	  { 
	  	any <<= (*iter); 
	  	anys[n++] = any; 
	  }
      val <<= anys;
      return TRUE;
      break;
    }
    case tyDoubleArray:
    {
      Any any;
      Anys anys;
      anys.length((*(DoubleArray*)ptr.m_pointer).size());
      int n = 0; DoubleArray::iterator iter = (*(DoubleArray*)ptr.m_pointer).begin();
      for (; iter != (*(DoubleArray*)ptr.m_pointer).begin(); iter++)
	  {
	  	any <<= (*iter); 
	  	anys[n++] = any; 
	  }
      val <<= anys;
      return TRUE;
      break;
    }
    default:
      return FALSE;
    }
}
#endif // defined(CDB_HAS_ANY)

// ----------------------[ Binary-Hex conversion ]-------------------------


#if !defined(CDB_HAS_ANY)

inline int strcasecmp(const String &a, const String &b)
{
  String c = "", d = "";
  unsigned int i;
  for(i = 0; i < a.length(); i++)
    c += toupper(a[i]);
  for(i = 0; i < b.length(); i++)
    d += toupper(b[i]);
  return c.compare(d);   
}

#endif // !defined(CDB_HAS_ANY)

// ========================================================================
//
// Auxiliary functions for converting binary data to strings of hexadecimal
// digits (0-9, A-F) and vice-versa.
//
// @@ These functions might be of use elsewhere, too, so consider moving
//    them to another module and making them public.
//
// ========================================================================

//
// Given a pointer to a chunk of data in memory and its size, produce a
// string of octets (bytes), representing them as hexadecimal numbers.
// The numbers (two digits each) are separated with commas.
//
inline String OctetsToHexString(const unsigned char *pData, int nSize)
{
  const char hex_tbl[16] =
    { '0', '1', '2', '3', '4', '5', '6', '7',
      '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };

  char *buf = new char[3*nSize];

  for(int i = 0; i < nSize; i++)
  {
    buf[3*i] = hex_tbl[pData[i] >> 4];
    buf[3*i+1] = hex_tbl[pData[i] & 0xF];
    buf[3*i+2] = ',';
  }

  buf[3*nSize-1] = '\0';

  String str(buf);
  delete buf;
  return str;
}

//
// Return the value of a hexadecimal digit (0-15). Case-insensitive.
//
inline char HexDigitToNum(char c)
{   
  if(c >= '0' && c <= '9')
    return c - '0';
  c = toupper(c);
  if(c >= 'A' && c <= 'F')
    return c - 'A' + 10;
  return -1;
}

//
// Given a string, convert it to a chunk of data in memory. The string is
// assumed to be a sequence of hexadecimal numbers, each occupying two
// characters, and two numbers being separated by another character (e.g.,
// comma). This function is the exact inverse of OctetsToHexString.
//
// Chunk of data is allocated by this function using new char[]. It is
// the caller's responsibility to later deallocate that memory.
//
// If conversion failed because the input string was malformed, a value
// of 0 is returned and the chunk of data is not allocated.
//
// EXAMPLE:
//
//       char *pData;
//       int nSize;
//       if(HexStringToOctets(str, pData, nSize))
//       {
//          // Do whatever is desired with pData and nSize...
//          . . .
//          delete[] pData;
//       }
//
inline Boolean HexStringToOctets(const String& str,
                              char *&pData,
                              int &nSize)
{
  Boolean bHi = 1;
  pData = new char[(str.length()+1)/2];
  char *pCur = pData;
  for(unsigned int i = 0; i < str.length(); ++i)
  {
    char n = HexDigitToNum(str[i]);
    if(n != (char)-1)
    {
      if(bHi)
        *pCur = n << 4;
      else
        *pCur++ |= n;
      bHi = !bHi;
    }
  }
  if(!bHi)
  {
    delete pData;
    pData = 0;
    nSize = 0;
  }    
  else
    nSize = pCur - pData;
  return bHi;
}

 }; 

// ========================================================================
//
// Stream extraction/insertion operators.
//
// ========================================================================

#if !defined(__GNUG__)

std::ostream& operator<<(std::ostream &os, LongLong ll)
{
  String s;
  if(ll < 0)
  {
    do
    {
      s = String('0' - ll % 10) + s;
      ll /= 10;
    } while(ll != 0);
    s = "-" + s;
  }
  else
  {
    do
    {
      s = String('0' + ll % 10) + s;
      ll /= 10;
    } while(ll != 0);
  }
  
  os << s;

  return os;
}

std::istream& operator>>(std::istream &is, LongLong &ll)
{
  ACE_TCHAR s[64];

  is >> s;

  ll = 0;

  unsigned int len = ACE_OS::strlen(s);
  for(unsigned int i = 0; i < len; i++)
    if(isdigit(s[i]))
      ll = ll*10 + s[i] - '0';

  if(s[0] == '-')
    ll *= -1;

  return is;
}

std::ostream& operator<<(std::ostream &os, ULongLong ll)
{   
  ACE_CString s;

  do 
  {
    s = String('0' + ll % 10) + s;
    ll /= 10;
  } while(ll != 0);

  os << s;

  return os;
}

std::istream& operator>>(std::istream &is, ULongLong &ll)
{
  ACE_TCHAR s[64];

  is >> s;

  ll = 0;

  unsigned int len = ACE_OS::strlen(s);
  for(unsigned int i = 0; i < len; i++)
    if(isdigit(s[i]))
      ll = ll*10 + s[i] - '0';

  return is;
}

#endif // !defined(__GNUG__)

#if defined(CDB_HAS_ANY)
#ifndef MAKE_VXWORKS
std::ostream& operator<<(std::ostream &os, const cdb::Any &any)
#else
std::ostream& operator<<(std::ostream &os, const Any &any)
#endif
{
  TAO_OutputCDR cdrOut(size_t(0), 1);

  cdrOut << any;
  os << OctetsToHexString((const unsigned char*)cdrOut.buffer(),
                          cdrOut.length());

  return os;
} 

#ifndef MAKE_VXWORKS
std::istream& operator>>(std::istream &is, cdb::Any &any)
#else
std::istream& operator>>(std::istream &is, Any &any)
#endif
{
  char *pData;
  int nSize;

  std::string str;
  is >> str;
  if(!HexStringToOctets(str.c_str(), pData, nSize))
  {
    is.setstate(ios::badbit);
  }

  TAO_InputCDR cdrIn(pData, nSize, 1);

  if(!(cdrIn >> any))
  {
    is.setstate(ios::badbit);
  }

  delete pData;

  return is;
}

#endif // defined(CDB_HAS_ANY)

namespace cdb {

//
// Split string str into several substrings, which are separated with
// commas. If quotes are used, then substrings are considered to be 
// enclosed in them. Quotes can be escaped so that they can be treated
// verbatim.
//
Boolean StringToStrings(const String str, StringArray &ary)
{

  // The string that will be added to the list next.
  String strCur; 
  // Tells us what kind of quote we are in.
  Boolean bQuote = 0;
  unsigned int iter = 0;
  unsigned int len = str.length();

  ary.clear();
  while(iter < len)
  {
    // We got to a whitespace and we are not in a quote: push the currently
    // build substring at the end of the array.
    if(!bQuote && str[iter] == ',')
    {
      if(strCur.length()!=0)
      {
        ary.push_back(strCur);
        strCur = "";
      }
    }
    // Escape sequence.
    else if(str[iter] == '\\')      
    {
      ++iter;
      // Whoops, escape ended before the new line.
      if(iter == len) 
      {
      	return 0;
      }
      switch(str[iter])
      {
      case 'n':
        strCur += '\n';
        break;
      case 'r':
        strCur += '\r';
        break;
      case ',':
      case '\\':
      case '\'':
      case '"':
        // Treat next character verbatim, regardless what it may be.
        strCur += str[iter];
        break;
      default:
        // An unrecognized escape!
        return 0;
      }
    }
    // The quote ended.
    else if(bQuote && str[iter] == '"')
    {
      // Indicate that we are in the quote no longer.
      bQuote = 0;
      ary.push_back(strCur);
      strCur = "";
    }
    // The quote begun.
    else if(str[iter] == '"')
    {
      if(strCur.length()!=0)
      {
        ary.push_back(strCur);
        strCur = "";
      }
      bQuote = 1;
    }
    else
      // A regular character.
      strCur += str[iter];
    ++iter;
  }

  // Push the last string to the end of the array.
  if(strCur.length()!=0)
  {
    ary.push_back(strCur);
  }

  return 1;
}

//
// Join several strings into a single one, separating them with spaces.
// If individual strings contain spaces, then they are quoted. Quotes
// are replaced with their corresponding escapes.
//
String StringsToString(const StringArray &ary)
{
  String str;
  StringArray::const_iterator iter;
  Boolean bQuoted;
  for(iter = ary.begin(); iter != ary.end(); ++iter)
  {
    bQuoted = (iter->find(',', 0) != String::npos);
    if(iter != ary.begin())
    {
      str += ',';
    }
    if(bQuoted)
    {
      str += '"';
    }

    for(unsigned int striter = 0; striter < iter->length(); ++striter)
      switch((*iter)[striter])
      {
      case '"': 
      	str += "\\\""; 
      	break;
      case '\\': 
      	str += "\\\\"; 
      	break;
      case '\r': 
      	str += "\\r"; 
      	break;
      case '\n': 
      	str += "\\n"; 
      	break;
      case '\t': 
      	str += "\\t"; 
      	break;
      default: 
      	str += (*iter)[striter];
      }
    if(bQuoted)
    {
      str += '"';
    }
  }
  return str;
}

const Field Field::Null(Field::tyNull, CDB_FIELD_NULL);
const Field Field::Nonexistent(Field::tyNull, CDB_FIELD_NONEXISTENT);

Field Field::operator[](ULong idx) const
{
  switch(m_ty)
  {
#if defined(CDB_HAS_ANY)
  case tyAnyArray:
    {
      AnyArray *pary = (AnyArray*)ptr.m_pointer;
      if(idx >= ((AnyArray*)ptr.m_pointer)->size()) 
      {
      	break;
      }
      return Field((*pary)[idx]);
    }
#endif // defined(CDB_HAS_ANY)
  case tyStringArray:
    {
      StringArray *pary = (StringArray*)ptr.m_pointer;
      if(idx >= ((StringArray*)ptr.m_pointer)->size()) 
      {
      	break;
      }
      return Field((*pary)[idx]);
    }
  case tyOctetArray:
    {
      OctetArray *pary = (OctetArray*)ptr.m_pointer;
      if(idx >= ((OctetArray*)ptr.m_pointer)->size()) 
      {
      	break;
      }
      return Field((*pary)[idx]);
    }
  case tyShortArray:
    {
      ShortArray *pary = (ShortArray*)ptr.m_pointer;
      if(idx >= ((ShortArray*)ptr.m_pointer)->size()) 
      {
      	break;
      }
      return Field((*pary)[idx]);
    }
  case tyLongArray:
    {
      LongArray *pary = (LongArray*)ptr.m_pointer;
      if(idx >= ((LongArray*)ptr.m_pointer)->size()) 
      {
      	break;
      }
      return Field((*pary)[idx]);
    }
  case tyLongLongArray:
    {
      LongLongArray *pary = (LongLongArray*)ptr.m_pointer;
      if(idx >= ((LongLongArray*)ptr.m_pointer)->size()) 
      {
      	break;
      }
      return Field((*pary)[idx]);
    }
  case tyUShortArray:
    {
      UShortArray *pary = (UShortArray*)ptr.m_pointer;
      if(idx >= ((UShortArray*)ptr.m_pointer)->size()) 
      {
      	break;
      }
      return Field((*pary)[idx]);
    }
  case tyULongArray:
    {
      ULongArray *pary = (ULongArray*)ptr.m_pointer;
      if(idx >= ((ULongArray*)ptr.m_pointer)->size()) 
      {
      	break;
      }
      return Field((*pary)[idx]);
    }
  case tyULongLongArray:
    {
      ULongLongArray *pary = (ULongLongArray*)ptr.m_pointer;
      if(idx >= ((ULongLongArray*)ptr.m_pointer)->size()) 
      {
      	break;
      }
      return Field((*pary)[idx]);
    }
  case tyFloatArray:
    {
      FloatArray *pary = (FloatArray*)ptr.m_pointer;
      if(idx >= ((FloatArray*)ptr.m_pointer)->size()) 
      {
      	break;
      }
      return Field((*pary)[idx]);
    }
  case tyDoubleArray:
    {
      DoubleArray *pary = (DoubleArray*)ptr.m_pointer;
      if(idx >= ((DoubleArray*)ptr.m_pointer)->size()) 
      {
      	break;
      }
      return Field((*pary)[idx]);
    }
  default:
    break;
  }
  return Field();
}

/*
Boolean Field::ToAny(Any &any) const
{
    if(m_ty == tyNull)
    {
        any = Any(CORBA::_tc_void);
        return 1;
    }
    if(m_ty == tyAny)
    {
        any = *((Any*)ptr.m_pointer);
        return 1;
    }

    if(m_ty == tyOctet)
    {
        any <<= CORBA::Any::from_octet(*((Octet*)m_inline));
        return 1;
    }

    if(m_ty == tyString)
    {
        any <<= ((String*)ptr.m_pointer)->c_str();
        return 1;
    }

#define PROCESS_TYPE(type)      \
    if(m_ty == ty##type)        \
    {                           \
        any <<= *((type*)m_inline);    \
        return 1;            \
    }
    PROCESS_TYPE(Short);
    PROCESS_TYPE(UShort);
    PROCESS_TYPE(Long);
    PROCESS_TYPE(ULong);
    PROCESS_TYPE(LongLong);
    PROCESS_TYPE(ULongLong);
    PROCESS_TYPE(Float);
    PROCESS_TYPE(Double);
//    PROCESS_TYPE(LongDouble);
#undef PROCESS_TYPE

   return 0;
}
*/

Boolean Field::ToString(String &str, Boolean bType) const
{
  unsigned int i;
  std::ostringstream os;

  switch(m_ty)
  {
  case tyNull:
    if (bType) 
    {
    	(ostream&)os << "<Null>" << '\0';
    }
    str = os.str().c_str();
    return 1;
#if defined(CDB_HAS_ANY)
  case tyAny:
    if (bType) 
    {
    	(ostream&)os << "<Any>";
    }
    (ostream&)os << *((Any*)ptr.m_pointer) << '\0';
    str = os.str().c_str();
    return 1;
#endif // defined(CDB_HAS_ANY)
  case tyString:
    if(bType)
    {
      os << "<String";
      if(ptr.m_ulBound != 0)
        (ostream&)os << ':' << ptr.m_ulBound;
      os << ">";
    }
    os << *((String*)ptr.m_pointer) << '\0';
    str = os.str().c_str();
    return 1;
  case tyOctet:
    if (bType) 
    {
    	os << "<Octet>";
    }
    (ostream&)os << UShort(*((Octet*)m_inline)) << '\0';
    str = os.str().c_str();
    return 1;
  case tyShort:
    if (bType) 
    {
    	os << "<Short>";
    }
    (ostream&)os << *((Short*)m_inline) << '\0';
    str = os.str().c_str();
    return 1;
  case tyLong:
    if (bType) 
    {
    	os << "<Long>";
    }
    (ostream&)os << *((Long*)m_inline) << '\0';
    str = os.str().c_str();
    return 1;
  case tyLongLong:
    if (bType) 
    {
    	os << "<LongLong>"; 
    }
    (ostream&)os << *((LongLong*)m_inline) << '\0';
    str = os.str().c_str();
    return 1;
  case tyUShort:
    if (bType) 
    {
    	os << "<UShort>";
    }
    (ostream&)os << *((UShort*)m_inline) << '\0';
    str = os.str().c_str();
    return 1;
  case tyULong:
    if (bType) 
    {
    	os << "<ULong>";
    }
    (ostream&)os << *((ULong*)m_inline) << '\0';
    str = os.str().c_str();
    return 1;
  case tyULongLong:
    if (bType) 
    {
    	os << "<ULongLong>";
    }
    (ostream&)os << *((ULongLong*)m_inline) << '\0';
    str = os.str().c_str();
    return 1;
  case tyFloat:
    if (bType) 
    {
    	os << "<Float>";
    }
    (ostream&)os << *((Float*)m_inline) << '\0';
    str = os.str().c_str();
    return 1;
  case tyDouble:
    if (bType) 
    {
    	os << "<Double>";
    }
    (ostream&)os << *((Double*)m_inline) << '\0';
    str = os.str().c_str();
    return 1;
  case tyStringArray:
    {
      StringArray *pary = (StringArray*)ptr.m_pointer;
      StringArray aStrings;
      for(i = 0; i < pary->size(); i++)
      {
        String str;
        (*this)[i].ToString(str, 0);
        aStrings.push_back(str);
      }
      if(bType)
      {
        (ostream&)os << "<StringArray";
        if(ptr.m_ulBound != 0)
        {
          (ostream&)os << ":" << ptr.m_ulBound;
        }
        (ostream&)os << ">";
      }
      (ostream&)os << StringsToString(aStrings) << '\0';
      str = os.str().c_str();
      return 1;
    }
  case tyOctetArray:
    {
      OctetArray *pary = (OctetArray*)ptr.m_pointer;
      StringArray aStrings;
      for(i = 0; i < pary->size(); i++)
      {
        String str;
        (*this)[i].ToString(str, 0);
        aStrings.push_back(str);
      }
      if(bType)
      {
        os << "<OctetArray";
        if(ptr.m_ulBound != 0)
        {
          os << ":" << ptr.m_ulBound;
        }
        os << ">";
      }
      os << StringsToString(aStrings) << '\0';
      str = os.str().c_str();
      return 1;
    }
#define PROCESS_TYPE(type) \
  case ty##type##Array: \
    {                                                   \
      type##Array *pary = (type##Array*)ptr.m_pointer;\
      StringArray aStrings;                             \
      for(i = 0; i < pary->size(); i++)                 \
      {                                                 \
        String str;                                     \
        (*this)[i].ToString(str, 0);                    \
        aStrings.push_back(str);                        \
      }                                                 \
      if(bType)                                         \
      {                                                 \
        os << "<" #type "Array";                        \
        if(ptr.m_ulBound != 0)                          \
        {                                               \
          os << ":" << ptr.m_ulBound;                   \
        }                                               \
        os << ">";                                      \
      }                                                 \
      os << StringsToString(aStrings) << '\0';          \
      str = os.str().c_str();                           \
      return 1;                                         \
    }


    PROCESS_TYPE(Short);
    PROCESS_TYPE(Long);
    PROCESS_TYPE(LongLong);
    PROCESS_TYPE(UShort);
    PROCESS_TYPE(ULong);
    PROCESS_TYPE(ULongLong);
    PROCESS_TYPE(Float);
    PROCESS_TYPE(Double);
  default:
    break;
#undef PROCESS_TYPE
  }
  return 0;
}

Boolean Field::FromString(const String &str)
{
  int nPos0 = str.find('<');
  int nPos1 = str.find(':');
  int nPos2 = str.find('>');
  unsigned long ulBound = 0;

  if(nPos1 != String::npos && nPos1 < nPos2-1)
  {
    ulBound = atoi(str.substr(nPos1+1, nPos2-nPos1-1).c_str());
  }
  else
  {
    nPos1 = nPos2;
  }

  String strType = str.substr(nPos0+1, nPos1-nPos0-1);
  String strContent = str.substr(nPos2+1);
  const char *szType = strType.c_str();
  const char *szContent = strContent.c_str();

  if(strcasecmp(szType, "Null") == 0)
  {
    return SetType(tyNull);
  }

#if defined(CDB_HAS_ANY)
  if(strcasecmp(szType, "Any") == 0)
  {
    if(!SetType(tyAny)) 
    {
    	return 0;
    }

    Any any;
    char *pData;
    int nSize;

    if(!HexStringToOctets(strContent, pData, nSize))
    {
      return 0;
    }

    TAO_InputCDR cdrIn(pData, nSize, 1);

    Boolean rv = 0;
    if(cdrIn >> any)
    {
      rv = SetAny(any);
    }

    delete pData;

    return rv;
  }
#endif // defined(CDB_HAS_ANY)

  if(strcasecmp(szType, "String") == 0)
  {
    if(ulBound != 0 && strContent.length() > ulBound) 
    {
    	return 0;
    }
    if(!SetType(tyString, ulBound)) 
    {
    	return 0;
    }
    return SetString(szContent);
  }

  if(strcasecmp(szType, "Octet") == 0)
  {
    std::istringstream is(szContent);
    int tmp;
    is >> tmp;
    if(!SetType(tyOctet)) 
    {
    	return 0;
    }
    return SetOctet(tmp);
  }

#define PROCESS_TYPE(t)                          \
  if(strcasecmp(szType, #t) == 0)                \
  {                                              \
    std::istringstream is(szContent);                    \
    t tmp;                                       \
    (istream&) is >> tmp;                        \
    if(!SetType(ty##t))                          \
    {                                            \
    	return 0;                                \
    }                                            \
    return Set##t(tmp);                          \
  }
  PROCESS_TYPE(Short);
  PROCESS_TYPE(Long);
  PROCESS_TYPE(LongLong);
  PROCESS_TYPE(UShort);
  PROCESS_TYPE(ULong);
  PROCESS_TYPE(ULongLong);
  PROCESS_TYPE(Float);
  PROCESS_TYPE(Double);
#undef PROCESS_TYPE

  if(strcasecmp(szType, "StringArray") == 0)
  {
    StringArray  ary;
    if(!StringToStrings(strContent, ary)) 
    {
    	return 0;
    }
    *this = Field(ary);
    return 1;
  }

  if(strcasecmp(szType, "OctetArray") == 0)
  {
    StringArray ary;
    OctetArray  aryOctet;

    if(!StringToStrings(strContent, ary)) 
    {
    	return 0;
    }
    for(StringArray::iterator iter = ary.begin();
        iter != ary.end();
        ++iter)
    {
      std::istringstream is(iter->c_str());
      UShort tmp;
      is >> tmp;
      aryOctet.push_back(tmp);
    }

    *this = Field(aryOctet);

    return 1;
  }

#define PROCESS_TYPE(type)                              \
  if(strcasecmp(szType, #type "Array") == 0)            \
  {                                                     \
    StringArray ary;                                    \
    type##Array  ary##type;                             \
                                                        \
    if(!StringToStrings(strContent, ary))               \
    {                                                   \
    	return 0;                                       \
    }                                                   \
    for(StringArray::iterator iter = ary.begin();       \
        iter != ary.end();                              \
        ++iter)                                         \
    {                                                   \
      std::istringstream is(iter->c_str());                     \
      type o;                                           \
      (istream&)is >> o;                                \
      ary##type.push_back(o);                           \
    }                                                   \
                                                        \
    *this = Field(ary##type);                           \
                                                        \
    return 1;                                           \
  }

  PROCESS_TYPE(Short);
  PROCESS_TYPE(Long);
  PROCESS_TYPE(LongLong);
  PROCESS_TYPE(UShort);
  PROCESS_TYPE(ULong);
  PROCESS_TYPE(ULongLong);
  PROCESS_TYPE(Float);
  PROCESS_TYPE(Double);
#undef PROCESS_TYPE

  return 0;
}

 }; 

#if defined(ACE_HAS_EXPLICIT_TEMPLATE_INSTANTIATION)

#define PROCESS_TYPE(type)                                       \
  template class vector<type>;                                   \
  template void fill(type *, type *, type const &);              \
  template type * fill_n(type *, unsigned int, type const &);

#if defined(CDB_HAS_ANY)
PROCESS_TYPE(Any);
#endif // defined(CDB_HAS_ANY)

PROCESS_TYPE(String);
//PROCESS_TYPE(Octet);
PROCESS_TYPE(Boolean);
PROCESS_TYPE(Short);
PROCESS_TYPE(Long);
PROCESS_TYPE(LongLong);
PROCESS_TYPE(UShort);
PROCESS_TYPE(ULong);
PROCESS_TYPE(ULongLong);
PROCESS_TYPE(Float);
PROCESS_TYPE(Double);

#elif defined (ACE_HAS_TEMPLATE_INSTANTIATION_PRAGMA)

#error Field.cpp does not support template instantiation pragma!

#endif // ACE_HAS_EXPLICIT_TEMPLATE_INSTANTIATION


// ------------------------------------------------------------------------


// --------------------------[ Constructors ]------------------------------

 Field::Field(Type ty, ULong nBound) :
  m_ty(tyNull), m_ulWhyNull(CDB_FIELD_UNINITIALIZED)
{
  SetType(ty, nBound);
}

Field::Field() :
  m_ty(tyNull), m_ulWhyNull(CDB_FIELD_UNINITIALIZED)
{
}

Field::Field(const Field& fld) :
  m_ty(tyNull), m_ulWhyNull(CDB_FIELD_UNINITIALIZED)
{
  // Delegate to the copy operator.
  *this = fld;
}

#if defined(CDB_HAS_ANY)
Field::Field(const Any &aAny) :
  m_ty(tyNull)
{
  SetType(tyAny);
  SetAny(aAny);
}
#endif // defined(CDB_HAS_ANY)

Field::Field(const String &aString) :
  m_ty(tyNull)
{
  SetType(tyString);
  SetString(aString);
}

Field::Field(Octet aOctet) :
  m_ty(tyNull)
{
  SetType(tyOctet);
  SetOctet(aOctet);
}

Field::Field(Short aShort) :
  m_ty(tyNull)
{
  SetType(tyShort);
  SetShort(aShort);
}

Field::Field(Long aLong) :
  m_ty(tyNull)
{
  SetType(tyLong);
  SetLong(aLong);
}

Field::Field(LongLong aLongLong) :
  m_ty(tyNull)
{
  SetType(tyLongLong);
  SetLongLong(aLongLong);
}

Field::Field(UShort aUShort) :
  m_ty(tyNull)
{
  SetType(tyUShort);
  SetUShort(aUShort);
}

Field::Field(ULong aULong)
  : m_ty(tyNull)
{
  SetType(tyULong);
  SetULong(aULong);
}

Field::Field(ULongLong aULongLong)
  : m_ty(tyNull)
{
  SetType(tyULongLong);
  SetULongLong(aULongLong);
}

Field::Field(Float aFloat)
  : m_ty(tyNull)
{
  SetType(tyFloat);
  SetFloat(aFloat);
}

Field::Field(Double aDouble)
  : m_ty(tyNull)
{
  SetType(tyDouble);
  SetDouble(aDouble);
}

#if defined(CDB_HAS_ANY)
Field::Field(const AnyArray &aAnyArray)
  : m_ty(tyNull)
{
  SetType(tyAnyArray);
  SetAnyArray(aAnyArray);
}
#endif // defined(CDB_HAS_ANY)

Field::Field(const StringArray &aStringArray)
  : m_ty(tyNull)
{
  SetType(tyStringArray);
  SetStringArray(aStringArray);
}

Field::Field(const OctetArray &aOctetArray)
  : m_ty(tyNull)
{
  SetType(tyOctetArray);
  SetOctetArray(aOctetArray);
}

Field::Field(const ShortArray &aShortArray)
  : m_ty(tyNull)
{
  SetType(tyShortArray);
  SetShortArray(aShortArray);
}

Field::Field(const LongArray &aLongArray)
  : m_ty(tyNull)
{
  SetType(tyLongArray);
  SetLongArray(aLongArray);
}

Field::Field(const LongLongArray &aLongLongArray)
  : m_ty(tyNull)
{
  SetType(tyLongLongArray);
  SetLongLongArray(aLongLongArray);
}

Field::Field(const UShortArray &aUShortArray)
  : m_ty(tyNull)
{
  SetType(tyUShortArray);
  SetUShortArray(aUShortArray);
}

Field::Field(const ULongArray &aULongArray)
  : m_ty(tyNull)
{
  SetType(tyULongArray);
  SetULongArray(aULongArray);
}

Field::Field(const ULongLongArray &aULongLongArray)
  : m_ty(tyNull)
{
  SetType(tyULongLongArray);
  SetULongLongArray(aULongLongArray);
}

Field::Field(const FloatArray &aFloatArray)
  : m_ty(tyNull)
{
  SetType(tyFloatArray);
  SetFloatArray(aFloatArray);
}

Field::Field(const DoubleArray &aDoubleArray)
  : m_ty(tyNull)
{
  SetType(tyDoubleArray);
  SetDoubleArray(aDoubleArray);
}

// ---------------------------[ Destructor ]-------------------------------

Field::~Field()
{
  SetType(tyNull); 
}

// -----------------[ Set type, assignment operator ]----------------------


// ----------------------------[ Accessors ]-------------------------------

Boolean Field::GetString(String &val) const
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
    {
      const char *sz;
      if(!(*(Any*)ptr.m_pointer >>= sz)) 
      {
      	return FALSE;
      }
      val = sz;
      return TRUE;
    }
#endif // defined(CDB_HAS_ANY)
  if(m_ty != tyString) 
  {
  	return FALSE;
  }
  val = *((String*)ptr.m_pointer);
  return TRUE;
}

Boolean Field::getValue(String &val) const
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
    {
      const char *sz;
      if(!(*(Any*)ptr.m_pointer >>= sz)) 
      {
      	return FALSE;
      }
      val = sz;
      return TRUE;
    }
#endif // defined(CDB_HAS_ANY)
  if(m_ty != tyString) 
  {
  	return FALSE;
  }
  val = *((String*)ptr.m_pointer);
  return TRUE;
}

Boolean Field::GetOctet(Octet &val) const
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
  {
    return (*(Any*)ptr.m_pointer >>= CORBA::Any::to_octet(val)) != 0;
  }
#endif // defined(CDB_HAS_ANY)
  if(m_ty != tyOctet) 
  {
  	return FALSE;
  }
  val = *((Octet*)m_inline);
  return TRUE;
}

Boolean Field::GetBoolean(Boolean &val) const
{
  switch(m_ty)
    {
#if defined(CDB_HAS_ANY)
    case tyAny:
      return (*(Any*)ptr.m_pointer >>= CORBA::Any::to_boolean(val)) != 0;
#endif // defined(CDB_HAS_ANY)
    case tyOctet:
      val = (*((Octet*)m_inline) != 0); 
      return TRUE;
    case tyShort:
      val = (*((Short*)m_inline) != 0); 
      return TRUE;
    case tyLong:
      val = (*((Long*)m_inline) != 0); 
      return TRUE;
    case tyLongLong:
      val = (*((LongLong*)m_inline) != 0); 
      return TRUE;
    case tyUShort:
      val = (*((UShort*)m_inline) != 0); 
      return TRUE;
    case tyULong:
      val = (*((ULong*)m_inline) != 0); 
      return TRUE;
    case tyULongLong:
      val = (*((LongLong*)m_inline) != 0); 
      return TRUE;
    case tyString:
      val = (atoi((*(String*)ptr.m_pointer).c_str()) != 0); 
      return TRUE;
    default:
      return FALSE;
    }
}

Boolean Field::GetShort(Short &val) const
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
  {
    return (*(Any*)ptr.m_pointer >>= val) != 0;
  }
#endif // defined(CDB_HAS_ANY)
  if(m_ty != tyShort) 
  {
  	return FALSE;
  }
  val = *((Short*)m_inline);
  return TRUE;
}

Boolean Field::GetLong(Long &val) const
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
  {
    return (*(Any*)ptr.m_pointer >>= val) != 0;
  }
#endif // defined(CDB_HAS_ANY)
  if(m_ty == tyLong)
  {
    val = *((Long*)m_inline);
  }
  else if ( Convert(val)) 
  {
      return TRUE;
  }
  else
  {
    return FALSE;
  }
  return TRUE;
}

Boolean Field::getValue(Long &val) const
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
  {
    return (*(Any*)ptr.m_pointer >>= val) != 0;
  }
#endif // defined(CDB_HAS_ANY)
  if(m_ty == tyLong)
  {
    val = *((Long*)m_inline);
  }
  else if ( Convert(val)) 
  {
     return TRUE;
  }
  else
  {
    return FALSE;
  }
  return TRUE;
}

Boolean Field::GetLongLong(LongLong &val) const
{  
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
  {
      return (*(Any*)ptr.m_pointer >>= val) != 0;
  }
#endif // defined(CDB_HAS_ANY)
  if(m_ty == tyLongLong)
  {
      val = *((LongLong*)m_inline);
  }
  else if ( Convert(val)) 
  {
      return TRUE;
  }
  else
  {
      return FALSE;
  }
  return TRUE;
/*
  if(m_ty != tyLongLong) return FALSE;
  val = *((LongLong*)m_inline);
  return TRUE;
*/
}

Boolean Field::GetUShort(UShort &val) const
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
  {
    return (*(Any*)ptr.m_pointer >>= val) != 0;
  }
#endif // defined(CDB_HAS_ANY)
  if(m_ty != tyUShort) 
  {
  	return FALSE;
  }
  val = *((UShort*)m_inline);
  return TRUE;
}

#define CONVERT_TYPE \
  if(m_ty != tyString)  \
  {                     \
    return FALSE;       \
  }                     \
  std::istringstream is(((String*)ptr.m_pointer)->c_str()); \
  (istream&) is >> val; \
  return TRUE; 

Boolean Field::Convert( Octet& val ) const { CONVERT_TYPE }
Boolean Field::Convert( Short& val ) const { CONVERT_TYPE }
Boolean Field::Convert( Long& val ) const { CONVERT_TYPE }
Boolean Field::Convert( LongLong& val ) const { CONVERT_TYPE }
Boolean Field::Convert( UShort& val ) const { CONVERT_TYPE }
Boolean Field::Convert( ULong& val ) const { CONVERT_TYPE }
Boolean Field::Convert( ULongLong& val ) const { CONVERT_TYPE }
Boolean Field::Convert( Float& val ) const { CONVERT_TYPE }
Boolean Field::Convert( Double& val ) const { CONVERT_TYPE }
#undef CONVERT_TYPE

Boolean Field::GetULong(ULong &val) const
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
  {
    return (*(Any*)ptr.m_pointer >>= val) != 0;
  }
#endif // defined(CDB_HAS_ANY)
  if(m_ty == tyLong)
  {
    val = *((Long*)m_inline);
  }
  else if (m_ty == tyULong) 
  {
    val = *((ULong*)m_inline);
  }
  else if( Convert(val) ) 
  {
    return TRUE;
  }
  else
  {
    return FALSE;
  }
  return TRUE;
}

Boolean Field::getValue(ULong &val) const
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
  {
    return (*(Any*)ptr.m_pointer >>= val) != 0;
  }
#endif // defined(CDB_HAS_ANY)
  if(m_ty == tyLong)
  {
    val = *((Long*)m_inline);
  }
  else if (m_ty == tyULong) 
  {
    val = *((ULong*)m_inline);
  }
  else if( Convert(val) ) {
    return TRUE;
  }
  else
  {
    return FALSE;
  }
  return TRUE;
}

Boolean Field::GetULongLong(ULongLong &val) const
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
  {
    return (*(Any*)ptr.m_pointer >>= val) != 0;
  }
#endif // defined(CDB_HAS_ANY)
 if(m_ty == tyULongLong)
 {
    val = *((ULongLong*)m_inline);
 }
  else if ( Convert(val)) 
  {
      return TRUE;
  }
  else
  {
    return FALSE;
  }
  return TRUE;
/*  if(m_ty != tyULongLong) return FALSE;
  val = *((ULongLong*)m_inline);
  return TRUE;
*/
}

Boolean Field::GetFloat(Float &val) const
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
  {
    return (*(Any*)ptr.m_pointer >>= val) != 0;
  }
#endif // defined(CDB_HAS_ANY)
  if(m_ty != tyFloat) {
    if( !Convert(val) ) 
    {
	  return FALSE;
    }
    return TRUE;
  }
  val = *((Float*)m_inline);
  return TRUE;
}

Boolean Field::getValue(Float &val) const
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
  {
    return (*(Any*)ptr.m_pointer >>= val) != 0;
  }
#endif // defined(CDB_HAS_ANY)
  if(m_ty != tyFloat) {
    if( !Convert(val) ) 
    {
	  return FALSE;
    }
    return TRUE;
  }
  val = *((Float*)m_inline);
  return TRUE;
}

Boolean Field::GetDouble(Double &val) const
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
  {
    return (*(Any*)ptr.m_pointer >>= val) != 0;
  }
#endif // defined(CDB_HAS_ANY)
  if(m_ty != tyDouble) {
    if( !Convert(val) ) 
    {
	  return FALSE;
    }
    return TRUE;
  }
  val = *((Double*)m_inline);
  return TRUE;
}


Boolean Field::getValue(Double &val) const
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
  {
    return (*(Any*)ptr.m_pointer >>= val) != 0;
  }
#endif // defined(CDB_HAS_ANY)
  if(m_ty != tyDouble) {
    if( !Convert(val) ) 
    {
	  return FALSE;
    }
    return TRUE;
  }
  val = *((Double*)m_inline);
  return TRUE;
}

#if defined(CDB_HAS_ANY)
Boolean Field::GetAnyArray(AnyArray &val) const
{
  if(m_ty != tyAnyArray) 
  {
  	return FALSE;
  }
  val = *((AnyArray*)ptr.m_pointer);
  return TRUE;
}
#endif // defined(CDB_HAS_ANY)

Boolean Field::Convert(StringArray &val) const
{
  if(m_ty != tyString) 
  {
    return FALSE; 
  }
  if(!StringToStrings(((String*)ptr.m_pointer)->c_str(), val))
  {
    return 0;
  }
  return 1;
}

Boolean Field::Convert(LongArray &val)  const
{
  StringArray arry;
  if( !Convert(arry ) )
  {
    return FALSE;
  }
  for(StringArray::iterator iter = arry.begin();       
      iter != arry.end();                              
      ++iter)                                         
  {                                                   
    std::istringstream is(iter->c_str());                     
    Long o;                                           
    (istream&)is >> o;                                
    val.push_back(o);                           
  }                                                   
  return 1;
}

Boolean Field::Convert(DoubleArray &val) const
{
  StringArray arry;
  if( !Convert(arry ) )
  {
    return FALSE;
  }
  for(StringArray::iterator iter = arry.begin();       
      iter != arry.end();                              
      ++iter)                                         
  {                                                   
    std::istringstream is(iter->c_str());                     
    Double o;                                           
    (istream&)is >> o;                                
    val.push_back(o);                           
  }                                                   
  return 1;
}

Boolean Field::GetStringArray(StringArray &val) const
{
  if(m_ty != tyStringArray) {
    if( Convert(val) )
    {
      return TRUE;
    }
    return FALSE;
  }
  val = *((StringArray*)ptr.m_pointer);
  return TRUE;
}

StringArray * Field::GetStringArray()
{
  if(m_ty != tyStringArray) {
    StringArray arry;
    if( Convert( arry ) ) {
      SetType(tyStringArray);
      SetStringArray(arry);
      return (StringArray*)ptr.m_pointer;
    }
    return 0;
  }
  return (StringArray*)ptr.m_pointer;
}


Boolean Field::GetOctetArray(OctetArray &val) const
{
  if(m_ty != tyOctetArray) 
  {
  	return FALSE;
  }
  val = *((OctetArray*)ptr.m_pointer);
  return TRUE;
}

Boolean Field::GetShortArray(ShortArray &val) const
{
  if(m_ty != tyShortArray) 
  {
  	return FALSE;
  }
  val = *((ShortArray*)ptr.m_pointer);
  return TRUE;
}

Boolean Field::GetLongArray(LongArray &val) const
{
  if(m_ty != tyLongArray) {
    if( Convert(val) )
    {
      return TRUE;
    }
    return FALSE;
  }
  val = *((LongArray*)ptr.m_pointer);
  return TRUE;
}

LongArray * Field::GetLongArray()
{
  if(m_ty != tyLongArray) {
    LongArray arry;
    if( Convert(arry) ) {
      SetType(tyLongArray);
      SetLongArray(arry);
      return (LongArray*)ptr.m_pointer;
    }
    return 0;
  }
  return (LongArray*)ptr.m_pointer;
}


Boolean Field::GetLongLongArray(LongLongArray &val) const
{
  if(m_ty != tyLongLongArray) 
  {
  	return FALSE;
  }
  val = *((LongLongArray*)ptr.m_pointer);
  return TRUE;
}

Boolean Field::GetUShortArray(UShortArray &val) const
{
  if(m_ty != tyUShortArray) 
  {
  	return FALSE;
  }
  val = *((UShortArray*)ptr.m_pointer);
  return TRUE;
}

Boolean Field::GetULongArray(ULongArray &val) const
{
  if(m_ty != tyULongArray) 
  {
  	return FALSE;
  }
  val = *((ULongArray*)ptr.m_pointer);
  return TRUE;
}

Boolean Field::GetULongLongArray(ULongLongArray &val) const
{
  if(m_ty != tyULongLongArray) 
  {
  	return FALSE;
  }
  val = *((ULongLongArray*)ptr.m_pointer);
  return TRUE;
}

Boolean Field::GetFloatArray(FloatArray &val) const
{
  if(m_ty != tyFloatArray) 
  {
  	return FALSE;
  }
  val = *((FloatArray*)ptr.m_pointer);
  return TRUE;
}

Boolean Field::GetDoubleArray(DoubleArray &val) const
{
  if(m_ty != tyDoubleArray) {
    if( Convert(val) )
    {
      return TRUE;
    }
    return FALSE;
  }
  val = *((DoubleArray*)ptr.m_pointer);
  return TRUE;
}

Boolean Field::getValue(DoubleArray &val) const
{
  if(m_ty != tyDoubleArray) {
    if( Convert(val) )
    {
      return TRUE;
    }
    return FALSE;
  }
  val = *((DoubleArray*)ptr.m_pointer);
  return TRUE;
}
// ----------------------------[ Mutators ]--------------------------------

#if defined(CDB_HAS_ANY)
Boolean Field::SetAny(const Any& val)
{
  if(m_ty != tyAny) 
  {
  	return FALSE;
  }
  *((Any*)ptr.m_pointer) = val; 
  return TRUE;
}
#endif // defined(CDB_HAS_ANY)

Boolean Field::SetString(const String& val)
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
    {
      *((Any*)ptr.m_pointer) <<= val.c_str();
      return TRUE;
    }
#endif // defined(CDB_HAS_ANY)
  if(m_ty != tyString || (ptr.m_ulBound != 0 && ptr.m_ulBound < val.length()))
  {
    return FALSE;
  }
  *((String*)ptr.m_pointer) = val; 
  return TRUE;
}

Boolean Field::SetOctet(Octet val)
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
    {
      *((Any*)ptr.m_pointer) <<= Any::from_octet(val);
      return TRUE;
    }
#endif // defined(CDB_HAS_ANY)
  if(m_ty != tyOctet) {
  	return FALSE;
  }
  *((Octet*)m_inline) = val; 
  return TRUE;
}

Boolean Field::SetBoolean(Boolean val)
{
  switch(m_ty)
    {
#if defined(CDB_HAS_ANY)
    case tyAny:
      *(Any*)ptr.m_pointer <<= Any::from_boolean(val);
      return TRUE;
#endif // defined(CDB_HAS_ANY)
    case tyOctet:
      return SetOctet((val)?1:0);
    case tyShort:
      return SetShort((val)?1:0);
    case tyLong:
      return SetLong((val)?1:0);
    case tyLongLong:
      return SetLongLong((val)?1:0);
    case tyUShort:
      return SetUShort((val)?1:0);
    case tyULong:
      return SetULong((val)?1:0);
    case tyULongLong:
      return SetULongLong((val)?1:0);
    case tyString:
      return SetString((val)?"1":"0");
    default:
      return FALSE;
    }
}

Boolean Field::SetShort(Short val)
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
    {
      *((Any*)ptr.m_pointer) <<= val;
      return TRUE;
    }
#endif // defined(CDB_HAS_ANY)
  if(m_ty != tyShort) 
  {
  	return FALSE;
  }
  *((Short*)m_inline) = val; 
  return TRUE;
}

Boolean Field::SetLong(Long val)
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
    {
      *((Any*)ptr.m_pointer) <<= val;
      return TRUE;
    }
#endif // defined(CDB_HAS_ANY)
  if(m_ty != tyLong) 
  {
  	return FALSE;
  }
  *((Long*)m_inline) = val; 
  return TRUE;
}

Boolean Field::SetLongLong(LongLong val)
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
    {
      *((Any*)ptr.m_pointer) <<= val;
      return TRUE;
    }
#endif // defined(CDB_HAS_ANY)
  if(m_ty != tyLongLong) 
  {
  	return FALSE;
  }
  *((LongLong*)m_inline) = val; 
  return TRUE;
}

Boolean Field::SetUShort(UShort val)
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
    {
      *((Any*)ptr.m_pointer) <<= val;
      return TRUE;
    }
#endif // defined(CDB_HAS_ANY)
  if(m_ty != tyUShort) 
  {
  	return FALSE;
  }
  *((UShort*)m_inline) = val; 
  return TRUE;
}

Boolean Field::SetULong(ULong val)
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
    {
      *((Any*)ptr.m_pointer) <<= val;
      return TRUE;
    }
#endif // defined(CDB_HAS_ANY)
  if(m_ty != tyULong) 
  {
  	return FALSE;
  }
  *((ULong*)m_inline) = val; 
  return TRUE;
}

Boolean Field::SetULongLong(ULongLong val)
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
    {
      *((Any*)ptr.m_pointer) <<= val;
      return TRUE;
    }
#endif // defined(CDB_HAS_ANY)
  if(m_ty != tyULongLong) 
  {
  	return FALSE;
  }
  *((ULongLong*)m_inline) = val; 
  return TRUE;
}

Boolean Field::SetFloat(Float val)
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
    {
      *((Any*)ptr.m_pointer) <<= val;
      return TRUE;
    }
#endif // defined(CDB_HAS_ANY)
  if(m_ty != tyFloat) 
  {
  	return FALSE;
  }
  *((Float*)m_inline) = val; 
  return TRUE;
}

Boolean Field::SetDouble(Double val)
{
#if defined(CDB_HAS_ANY)
  if(m_ty == tyAny)
    {
      *((Any*)ptr.m_pointer) <<= val;
      return TRUE;
    }
#endif // defined(CDB_HAS_ANY)
  if(m_ty != tyDouble) 
  {
  	return FALSE;
  }
  *((Double*)m_inline) = val; 
  return TRUE;
}

#if defined(CDB_HAS_ANY)
Boolean Field::SetAnyArray(const AnyArray &val)
{
  if(m_ty != tyAnyArray) 
  {
  	return FALSE;
  }
  *((AnyArray*)ptr.m_pointer) = val; 
  return TRUE;
}
#endif // defined(CDB_HAS_ANY)

Boolean Field::SetStringArray(const StringArray &val)
{
  if(m_ty != tyStringArray) 
  {
  	return FALSE;
  }
  *((StringArray*)ptr.m_pointer) = val; 
  return TRUE;
}

Boolean Field::SetOctetArray(const OctetArray &val)
{
  if(m_ty != tyOctetArray) 
  {
  	return FALSE;
  }
  *((OctetArray*)ptr.m_pointer) = val; 
  return TRUE;
}

Boolean Field::SetShortArray(const ShortArray &val)
{
  if(m_ty != tyShortArray) 
  {
  	return FALSE;
  }
  *((ShortArray*)ptr.m_pointer) = val; 
  return TRUE;
}

Boolean Field::SetLongArray(const LongArray &val)
{
  if(m_ty != tyLongArray) 
  {
  	return FALSE;
  }
  *((LongArray*)ptr.m_pointer) = val; 
  return TRUE;
}

Boolean Field::SetLongLongArray(const LongLongArray &val)
{
  if(m_ty != tyLongLongArray) 
  {
  	return FALSE;
  }
  *((LongLongArray*)ptr.m_pointer) = val; 
  return TRUE;
}

Boolean Field::SetUShortArray(const UShortArray &val)
{
  if(m_ty != tyUShortArray) 
  {
  	return FALSE;
  }
  *((UShortArray*)ptr.m_pointer) = val; 
  return TRUE;
}

Boolean Field::SetULongArray(const ULongArray &val)
{
  if(m_ty != tyULongArray) 
  {
  	return FALSE;
  }
  *((ULongArray*)ptr.m_pointer) = val; 
  return TRUE;
}

Boolean Field::SetULongLongArray(const ULongLongArray &val)
{
  if(m_ty != tyULongLongArray) 
  {
  	return FALSE;
  }
  *((ULongLongArray*)ptr.m_pointer) = val; 
  return TRUE;
}

Boolean Field::SetFloatArray(const FloatArray &val)
{
  if(m_ty != tyFloatArray) 
  {
  	return FALSE;
  }
  *((FloatArray*)ptr.m_pointer) = val; 
  return TRUE;
}

Boolean Field::SetDoubleArray(const DoubleArray &val)
{
  if(m_ty != tyDoubleArray) 
  {
  	return FALSE;
  }
  *((DoubleArray*)ptr.m_pointer) = val; 
  return TRUE;
}


// ************************************************************************
//
// REVISION HISTORY:
//
//   $Log: cdbField.cpp,v $
//   Revision 1.32  2008/07/25 07:24:24  cparedes
//   Removing use namespace from included files
//
//   Revision 1.31  2006/09/01 02:20:54  cparedes
//   small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
//
//   Revision 1.30  2005/08/23 15:23:27  vwang
//   to add float into BACI
//
//   Revision 1.29  2005/07/25 15:16:33  vwang
//   change strstream to sstream to elimite warning message
//
//   Revision 1.28  2005/02/14 10:39:36  acaproni
//   Some changes to reduce the coding standards number of errors
//
//   Revision 1.27  2003/08/18 12:36:00  rgeorgie
//   LGPL header added
//
//   Revision 1.26  2003/07/10 15:23:16  bjeram
//   support for longlong and uLongLong
//
//   Revision 1.25  2003/07/09 08:07:35  bjeram
//   ported to gcc 3.2
//
//   Revision 1.24  2003/01/28 16:44:04  vltsccm
//   gchiozzi: patch for cdb module to create lib/endorsed directory, since CVS cannot restore empty directories
//
//   Revision 1.23  2003/01/24 10:44:26  vltsccm
//   cdb1.23
//
//   Revision 1.22  2003/01/20 15:12:33  vltsccm
//   cdb1.22
//
//   Revision 1.21  2003/01/20 10:46:07  vltsccm
//   cdb1.21
//
//   Revision 1.20  2002/12/05 16:04:12  vltsccm
//   cdb1.20
//
//   Revision 1.19  2002/11/25 16:05:05  vltsccm
//   cdb1.19
//
//   Revision 1.18  2002/11/13 14:53:17  vltsccm
//   cdb1.18
//
//   Revision 1.17  2002/11/13 10:22:43  vltsccm
//   cdb1.17
//
//   Revision 1.16  2002/11/06 08:37:39  vltsccm
//   cdb1.16
//
//   Revision 1.15.1.23  2002/11/05 16:05:27  vltsccm
//   cdb1.15.1.23
//
//   Revision 1.15.1.22  2002/11/05 13:46:45  vltsccm
//   cdb1.15.1.22
//
//   Revision 1.15.1.21  2002/11/05 10:41:27  vltsccm
//   cdb1.15.1.21
//
//   Revision 1.15.1.20  2002/11/01 12:49:16  vltsccm
//   cdb1.15.1.20
//
//   Revision 1.15.1.19  2002/10/30 07:56:57  vltsccm
//   cdb1.15.1.19
//
//   Revision 1.15.1.18  2002/10/25 12:44:40  vltsccm
//   cdb1.15.1.18
//
//   Revision 1.15.1.17  2002/10/24 13:08:57  vltsccm
//   cdb1.15.1.17
//
//   Revision 1.15.1.16  2002/10/16 11:44:29  vltsccm
//   cdb1.15.1.16
//
//   Revision 1.15.1.15  2002/10/14 22:26:33  vltsccm
//   cdb1.15.1.15
//
//   Revision 1.15.1.14  2002/10/14 12:18:58  vltsccm
//   cdb1.15.1.14
//
//   Revision 1.15.1.13  2002/10/04 16:20:39  vltsccm
//   cdb1.15.1.13
//
//   Revision 1.15.1.12  2002/10/02 12:54:26  vltsccm
//   cdb1.15.1.12
//
//   Revision 1.15.1.11  2002/10/01 10:33:37  vltsccm
//   cdb1.15.1.11
//
//   Revision 1.15.1.10  2002/09/30 13:57:46  vltsccm
//   cdb1.15.1.10
//
//   Revision 1.15.1.9  2002/09/26 14:13:16  vltsccm
//   cdb1.15.1.9
//
//   Revision 1.15.1.8  2002/09/26 07:47:07  vltsccm
//   cdb1.15.1.8
//
//   Revision 1.15.1.7  2002/09/17 16:19:25  vltsccm
//   cdb1.15.1.7
//
//   Revision 1.15.1.6  2002/09/17 11:15:51  vltsccm
//   cdb1.15.1.6
//
//   Revision 1.15.1.5  2002/09/02 09:37:09  vltsccm
//   cdb1.15.1.5
//
//   Revision 1.15.1.4  2002/08/09 09:35:27  vltsccm
//   cdb1.15.1.4
//
//   Revision 1.15.1.3  2002/07/24 07:29:13  vltsccm
//   cdb1.15.1.3
//
//   Revision 1.15.1.2  2002/07/12 09:58:21  vltsccm
//   cdb1.15.1.2
//
//   Revision 1.15+.1.1  2002/07/09 09:40:33  vltsccm
//   cdb1.15.1
//
//   Revision 1.15  2002/02/05 17:50:09  vltsccm
//   cdb1.15
//
//   Revision 1.14  2002/01/14 21:14:20  vltsccm
//   cdb1.14
//
//   Revision 1.13  2001/10/19 09:56:24  vltsccm
//   cdb1.13
//
//   Revision 1.12  2001/09/18 10:07:14  vltsccm
//   cdb1.12
//
//   Revision 1.11  2001/07/12 07:48:30  vltsccm
//   cdb1.11
//
//   Revision 1.10  2001/07/11 09:16:28  vltsccm
//   cdb1.10
//
//   Revision 1.6  2000/12/07 18:00:42  vltsccm
//   cdb1.6
//
//   Revision 1.5  2000/11/17 13:14:59  vltsccm
//   cdb1.5
//
//   Revision 1.4  2000/10/20 13:51:33  vltsccm
//   cdb1.4
//
//   Revision 1.3  2000/10/20 13:51:33  vltsccm
//   cdb1.3
//
//   Revision 1.2  2000/10/20 13:51:32  vltsccm
//   cdb1.2
//
//   Revision 1.1  2000/10/20 13:51:32  vltsccm
//   cdb1.1
//
//   Revision 1.0  2000/10/20 13:51:32  vltsccm
//   cdb1.0
//
//   Revision 1.3  2000/10/13 16:03:05  vltsccm
//   cdb1.3
//
//   Revision 1.2  2000/09/13 14:49:31  vltsccm
//   cdb1.2
//
//   Revision 1.1  2000/09/06 15:42:13  vltsccm
//   cdb1.1
//
//   Revision 1.1  2000/08/12 12:21:28  matej
//   *** empty log message ***
//
//   Revision 1.2  2000/07/05 12:14:21  matej
//   *** empty log message ***
//
//   Revision 1.1  2000/06/13 07:26:24  kzagar
//   CDB, initial commit. Documentation not yet finished.
//
// ************************************************************************

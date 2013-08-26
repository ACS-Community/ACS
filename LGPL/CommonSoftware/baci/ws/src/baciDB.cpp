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
* "@(#) $Id: baciDB.cpp,v 1.100 2008/10/01 02:26:45 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/06/23  redesigned to OO design
* msekoran  2001/02/17  created 
*/

#include <vltPort.h>

#include "baciDB.h"
#include <logging.h>

 using namespace cdb;
 using namespace baci;

ACE_CString DBConnector::dbPrefix_m = "";
Table * DBConnector::dbTable_mp = 0;

/* ------------------------------------------------------------------ */
/*                          BACI helpers                              */
/* ------------------------------------------------------------------ */

/* ------------------ [ CharacteristicModel interface ] ------------------ */

CORBA::Any *
DBConnector::get_characteristic_by_name (const ACE_CString& objectName,
					 const char * name)
{
  CORBA::Any * value_p = new CORBA::Any();

  Table * table_p = DBConnector::getDBTable();
  if (table_p==0)
      {
      ACE_THROW_RETURN(CORBA::NO_RESOURCES(), 0);
      }
  ACE_CString propertyName = DBConnector::getFullAddress(objectName);

  Field field;
  if (table_p->GetField(propertyName, name, field)==false)
    {
      ACS::NoSuchCharacteristic * nsc_p = new ACS::NoSuchCharacteristic();
      nsc_p->characteristic_name = CORBA::string_dup(name);
      nsc_p->component_name = CORBA::string_dup(objectName.c_str());
      ACE_THROW_RETURN(ACS::NoSuchCharacteristic(), value_p);
    }

  field.GetAny(*value_p);
  return value_p;
}

ACS::stringSeq *
DBConnector::find_characteristic (const ACE_CString& name,
				  const char * reg_exp)
{
  Table * table_p = DBConnector::getDBTable();
  if (table_p==0)
      {
      ACE_THROW_RETURN(CORBA::NO_RESOURCES(), 0);
      }
  ACE_CString propertyName = DBConnector::getFullAddress(name);

  Field fld;
  StringArray * arry_p;

  if (table_p->GetField(propertyName, "_characteristics", fld)==false)
      {
      ACS_LOG(LM_RUNTIME_CONTEXT, "baci::DBConnector::find_characteristic", 
	      (LM_ERROR, "Misconfigured configuration database "
	       "(record '%s' field '_characteristics'): "
	       "An array of strings expected", propertyName.c_str()));
      ACE_THROW_RETURN(CORBA::NO_RESOURCES(), 0);
      }
  
  arry_p = fld.GetStringArray();
  
  int maxSize = 0;

  // count all matching non-null strings
  StringArray::const_iterator iter = arry_p->begin();
  for(; (iter != arry_p->end()) && (iter->length() > 0); ++iter)
      {
      maxSize++;  
      }

  // fill with names
  ACS::stringSeq_var strSeq = new ACS::stringSeq(maxSize);
  strSeq->length(maxSize);
  
  unsigned int n=0UL;
  for(iter = arry_p->begin(); (iter != arry_p->end()) && (iter->length() > 0); ++iter)
      {
      if (Wildcard::wildcardfit(reg_exp, iter->c_str())==1)
	  {
	  strSeq[n++] = CORBA::string_dup(iter->c_str());
	  }
      }
  strSeq->length(n);
  return strSeq._retn();
}

CosPropertyService::PropertySet_ptr
DBConnector::get_all_characteristics (const ACE_CString& name)
{
  if (CDBPropertySet::getInstance()==0)
      {
      ACE_THROW_RETURN(CORBA::NO_RESOURCES(), CosPropertyService::PropertySet::_nil());
      }
  return CDBPropertySet::getInstance()->getPropertySet(name.c_str());
}


/* ------------------------------------------------------------------ */
/* ------------------------------------------------------------------ */
/* ------------------------------------------------------------------ */

// for simulation
void
DBConnector::writeCommand(ACE_CString deviceName, ACE_CString commandDesc, ACE_CString commandTimestamp)
{
  ACE_CString name = getFullAddress(deviceName);
    
  Field fld(commandDesc);
  getDBTable()->SetField(name, "recentCommand", fld, FALSE);

  fld.SetString(commandTimestamp);
  getDBTable()->SetField(name, "recentTimeStamp", fld, FALSE);
    
}

void
DBConnector::writeString(ACE_CString name, const ACE_CString& value)
{
  name = getFullAddress(name);
  Field fld(value);
  getDBTable()->SetField(name, "value", fld, FALSE);
}

void
DBConnector::writeDouble(ACE_CString name, CORBA::Double value)
{
  name = getFullAddress(name);
  Field fld(value);
  getDBTable()->SetField(name, "value", fld, FALSE);
}

void
DBConnector::writeLong(ACE_CString name, CORBA::Long value)
{
  name = getFullAddress(name);
  Field fld((Long)value);
  getDBTable()->SetField(name, "value", fld, FALSE);
}

void
DBConnector::writeULong(ACE_CString name, CORBA::ULong value)
{
  name = getFullAddress(name);
  Field fld((ULong)value);
  getDBTable()->SetField(name, "value", fld, FALSE);
}

ACE_CString
DBConnector::readString(ACE_CString name)
{
  name = getFullAddress(name);
  Field fld;
  if (getDBTable()->GetField(name, "value", fld)==false)
      {
      return "error reading from DB";
      }
  ACE_CString str;
  fld.GetString(str);
  return str;
}

CORBA::Double
DBConnector::readDouble(ACE_CString name)
{
  name = getFullAddress(name);
  Field fld;
  if (getDBTable()->GetField(name, "value", fld)==false)
      {
      return 0.0;
      }
  double dbl;
  fld.GetDouble(dbl);
  return dbl;
}

CORBA::Long
DBConnector::readLong(ACE_CString name)
{
  name = getFullAddress(name);
  Field fld;
  if (getDBTable()->GetField(name, "value", fld)==false) 
      {
      return 0;
      }
  Long lng;
  fld.GetLong(lng);
  return lng;
}

CORBA::ULong
DBConnector::readULong(ACE_CString name)
{
  name = getFullAddress(name);
  Field fld;
  if (getDBTable()->GetField(name, "value", fld)==false) 
      {
      return 0;
      }
  ULong lng;
  fld.GetULong(lng);
  return lng;
}

ACS::doubleSeq
DBConnector::readDoubleSeq(ACE_CString name)
{
  ACS::doubleSeq val;
  val.length(0);
  name = getFullAddress(name);
  Field fld;
  if (getDBTable()->GetField(name, "value", fld)==false)
      {
      return val;
      }
  DoubleArray dblArray;
  if (fld.GetDoubleArray(dblArray)==true)
  {
      int n = 0;
      val.length(dblArray.size());
      for (DoubleArray::iterator iter = dblArray.begin();
	   iter != dblArray.end();
	   iter++)
	  {
	  val[n++] = *iter;
	  }
  }
  return val;
}

ACS::longSeq
DBConnector::readLongSeq(ACE_CString name)
{
  ACS::longSeq val;
  val.length(0);
  name = getFullAddress(name);
  Field fld;
  if (getDBTable()->GetField(name, "value", fld)==false) 
      {
      return val;
      }
  LongArray lngArray;
  if (fld.GetLongArray(lngArray)==true)
  {
      int n = 0;
      val.length(lngArray.size());
      for (LongArray::iterator iter = lngArray.begin();
	   iter != lngArray.end();
	   iter++)
	  {
	  val[n++] = *iter;
	  }
  }
  return val;
}

void
DBConnector::writeDoubleSeq(ACE_CString name, const ACS::doubleSeq & val)
{
    DoubleArray dblArray;
    for (CORBA::ULong i=0; i < val.length(); i++)
	{
	dblArray.push_back(val[i]);
	}
    name = getFullAddress(name);
    Field fld(dblArray);
    getDBTable()->SetField(name, "value", fld, FALSE);
}

void
DBConnector::writeLongSeq(ACE_CString name, const ACS::longSeq & val)
{
  LongArray lngArray;
  for (CORBA::ULong i=0; i < val.length(); i++)
      {
      lngArray.push_back(val[i]);
      }
  name = getFullAddress(name);
  Field fld(lngArray);
  getDBTable()->SetField(name, "value", fld, FALSE);
}

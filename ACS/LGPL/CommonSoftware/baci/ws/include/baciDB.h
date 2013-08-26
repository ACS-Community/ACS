#ifndef baciDB_H
#define baciDB_H

/*******************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2003 
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
* "@(#) $Id: baciDB.h,v 1.100 2010/04/20 13:58:41 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/06/23  redesigned to OO design
* msekoran  2001/03/07  modified
*/

/** 
 * @file 
 * Header file BACI Database Access.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>
#include <baciExport.h>
#include <ace/SString.h>
#include <tao/corba.h>

#include <baciC.h>
#include <acsutilWildcard.h>
#include <baciCDBPropertySet.h>

#include <cdb.h>

namespace baci {

class baci_EXPORT DBConnector
{

public:

  /// Create and initialize CDB Table instance 
  static bool initDB(const ACE_CString& dbPrefix, int argc = 0, char** argv = NULL, CORBA::ORB_ptr orb = CORBA::ORB::_nil())
  {
    dbPrefix_m = dbPrefix;
    dbTable_mp = cdb::getDatabase(argc, argv, orb);

    return (dbTable_mp!=0);
  }
  
  
  /// Close DB and destroy instance
  static void closeDB()
  {
    if (dbTable_mp!=0)
      {
	destroyDatabase(dbTable_mp);
	dbTable_mp = 0;
      }
  }

  /// Generate full address (by adding prefix to address)
  static ACE_CString getFullAddress(const ACE_CString& address)
  {
    return dbPrefix_m+"/"+address;
  };

  /// Get CDB Table instance
  static cdb::Table * getDBTable()
  { 
    return dbTable_mp;
  }


  ///
  /// BACI helpers
  ///

  /* ------------------ [ CharacteristicModel interface ] ------------------ */

  /*
  * @throw ACS::NoSuchCharacteristic
  */
  static CORBA::Any * get_characteristic_by_name (
						  const ACE_CString& objectName,
						  const char * name
						  
						  );
	
  static ACS::stringSeq * find_characteristic (
					       const ACE_CString& name,
					       const char * reg_exp
					       
					       );

  static CosPropertyService::PropertySet_ptr get_all_characteristics (
								      const ACE_CString& name
								      
      );


  /* ------------------------------------------------------------------ */

  /// for simulation !!!
  /// Write command string to DB including given timestamp
  /// This affects "recentCommand" and "commandTimestamp" fields
  static void writeCommand(ACE_CString deviceName, ACE_CString commandDesc, ACE_CString commandTimestamp);
  
  /// Read string type 
  static void writeString(ACE_CString name, const ACE_CString& value);

  /// Write CORBA::Double type 
  static void writeDouble(ACE_CString name, CORBA::Double value);

  /// Write CORBA::Long type 
  static void writeLong(ACE_CString name, CORBA::Long value);

  /// Write CORBA::ULong type 
  static void writeULong(ACE_CString name, CORBA::ULong value);

  /// Read ACS::doubleSeq type 
  static void writeDoubleSeq(ACE_CString name, const ACS::doubleSeq & val);

  /// Read ACS::longSeq type 
  static void writeLongSeq(ACE_CString name, const ACS::longSeq & val);

  /// Read string type 
  static ACE_CString readString(ACE_CString name);

  /// Read CORBA::Double type 
  static CORBA::Double readDouble(ACE_CString name);

  /// Read ACS::doubleSeq type 
  static ACS::doubleSeq readDoubleSeq(ACE_CString name);

  /// Read ACS::longSeq type 
  static ACS::longSeq readLongSeq(ACE_CString name);

  /// Read CORBA::Long type 
  static CORBA::Long readLong(ACE_CString name);

  /// Read CORBA::ULong type 
  static CORBA::ULong readULong(ACE_CString name);

private:

  /// DB address prefix
  static ACE_CString dbPrefix_m;

  /// CDB instance
  static cdb::Table * dbTable_mp;
  
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const DBConnector&);

    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    DBConnector(const DBConnector&);

};

 }; 

#endif   /* baciDB_H */




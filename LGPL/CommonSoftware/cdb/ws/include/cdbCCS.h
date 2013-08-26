#ifndef __cdb_CCS_h__
#define __cdb_CCS_h__
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
* "@(#) $Id: cdbCCS.h,v 1.26 2011/10/28 15:05:05 hsommer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2011-10-28  created
*/

#include "cdb.h"

namespace cdb {

class CCS : public Table
{

private:
  Boolean isInit;

  ACE_Recursive_Thread_Mutex cdbMutex;
  
public:
/**
Class to intialize access to ACS configuration database. 
It is necessary to create an instance of this class before 
any access to the ACS configuration database can be done
*/
  CCS(String process);

/**
CCS class destructor
*/
  virtual ~CCS();
	static  Table* createTable( int argc, char** argv, CORBA::ORB_ptr orb );

/**
Method to check if the access to the database is initialized.
*/
  virtual Boolean isInitialized() { return isInit; }
  
  Boolean CreateRecord(const String &strRecordName,
                       Boolean bTruncate = FALSE);

  ULong GetRecordState(const String &strRecordName);

/**
Get data from ACS configuration database.

@param strRecordName  name of the database record
@param strFieldName   name of the field in the record
@param fld            data returned

EXAMPLE
     #include "cdbCCS.h"
    
     double val;
     Field fld;
     DBManager::myClass->GetField("Appl_data:Mount:cmdAz","min_value",fld);
     fld.GetDouble(val);
*/
  Boolean GetField(const String &strRecordName,
                   const String &strFieldName,
                   Field &fld);

/**
Set field in the ACS configuration database.

@param strRecordName  name of the database record
@param strFieldName   name of the field in the record
@param fld            data returned
@param bCreate        creat flag, not used in the present implementation

EXAMPLE
     #include "cdbCCS.h"

     DBManager::myClass->SetField("Appl_data:Mount:cmdAz","value",
                                  Field(Double(1.5)),FALSE);
*/
  Boolean SetField(const String &strRecordName,
                   const String &strFieldName,
                   const Field &fld,
                   Boolean bCreate = TRUE);

  Boolean RemoveField(const String &strRecordName,
                      const String &strFieldName);

  Boolean GetRecord(const String &strRecordName,
                    Record &rec,
                    Boolean bCreate = FALSE,
                    Boolean bAppend = FALSE);
  
  Boolean SetRecord(const String &strRecordName,
                    const Record &rec,
                    Boolean bCreate = TRUE,
                    Boolean bAll = TRUE);
  Boolean RemoveRecord(const String &strRecordName);

  // ----------------------------------------------------------------------
  // GROUP = Navigation
  // ----------------------------------------------------------------------

  //
  // DESCRIPTION: Get all child-records of a given record.
  //
  // PARAMETERS:
  //   strRecordName
  //
  virtual Boolean GetChildren(const String &strRecordName,
                              StringArray &astrChildren);
};

 }; 

#endif // __cdb_CCS_h__

// ************************************************************************
//
// REVISION HISTORY:
//
//   $Log: cdbCCS.h,v $
//   Revision 1.26  2011/10/28 15:05:05  hsommer
//   Manually fixed "no LGPL license text" issue reported by addCopyright.py
//
//   Revision 1.25  2006/09/01 02:20:54  cparedes
//   small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
//
//   Revision 1.24  2003/01/28 16:43:49  vltsccm
//   gchiozzi: patch for cdb module to create lib/endorsed directory, since CVS cannot restore empty directories
//
//   Revision 1.23  2003/01/24 10:44:02  vltsccm
//   cdb1.23
//
//   Revision 1.22  2003/01/20 15:12:19  vltsccm
//   cdb1.22
//
//   Revision 1.21  2003/01/20 10:45:52  vltsccm
//   cdb1.21
//
//   Revision 1.20  2002/12/05 16:03:58  vltsccm
//   cdb1.20
//
//   Revision 1.19  2002/11/25 16:04:49  vltsccm
//   cdb1.19
//
//   Revision 1.18  2002/11/13 14:53:04  vltsccm
//   cdb1.18
//
//   Revision 1.17  2002/11/13 10:22:30  vltsccm
//   cdb1.17
//
//   Revision 1.16  2002/11/06 08:37:03  vltsccm
//   cdb1.16
//
//   Revision 1.15.1.23  2002/11/05 16:05:12  vltsccm
//   cdb1.15.1.23
//
//   Revision 1.15.1.22  2002/11/05 13:46:30  vltsccm
//   cdb1.15.1.22
//
//   Revision 1.15.1.21  2002/11/05 10:41:13  vltsccm
//   cdb1.15.1.21
//
//   Revision 1.15.1.20  2002/11/01 12:49:02  vltsccm
//   cdb1.15.1.20
//
//   Revision 1.15.1.19  2002/10/30 07:56:43  vltsccm
//   cdb1.15.1.19
//
//   Revision 1.15.1.18  2002/10/25 12:44:23  vltsccm
//   cdb1.15.1.18
//
//   Revision 1.15.1.17  2002/10/24 13:08:43  vltsccm
//   cdb1.15.1.17
//
//   Revision 1.15.1.16  2002/10/16 11:43:44  vltsccm
//   cdb1.15.1.16
//
//   Revision 1.15.1.15  2002/10/14 22:26:09  vltsccm
//   cdb1.15.1.15
//
//   Revision 1.15.1.14  2002/10/14 12:18:32  vltsccm
//   cdb1.15.1.14
//
//   Revision 1.15.1.13  2002/10/04 16:20:22  vltsccm
//   cdb1.15.1.13
//
//   Revision 1.15.1.12  2002/10/02 12:54:14  vltsccm
//   cdb1.15.1.12
//
//   Revision 1.15.1.11  2002/10/01 10:33:24  vltsccm
//   cdb1.15.1.11
//
//   Revision 1.15.1.10  2002/09/30 13:56:51  vltsccm
//   cdb1.15.1.10
//
//   Revision 1.15.1.9  2002/09/26 14:13:10  vltsccm
//   cdb1.15.1.9
//
//   Revision 1.15.1.8  2002/09/26 07:45:46  vltsccm
//   cdb1.15.1.8
//
//   Revision 1.15.1.7  2002/09/17 16:19:21  vltsccm
//   cdb1.15.1.7
//
//   Revision 1.15.1.6  2002/09/17 11:15:47  vltsccm
//   cdb1.15.1.6
//
//   Revision 1.15.1.5  2002/09/02 09:37:06  vltsccm
//   cdb1.15.1.5
//
//   Revision 1.15.1.4  2002/08/09 09:35:22  vltsccm
//   cdb1.15.1.4
//
//   Revision 1.15.1.3  2002/07/24 07:29:10  vltsccm
//   cdb1.15.1.3
//
//   Revision 1.15.1.2  2002/07/12 09:58:16  vltsccm
//   cdb1.15.1.2
//
//   Revision 1.15+.1.1  2002/07/09 09:40:08  vltsccm
//   cdb1.15.1
//
//   Revision 1.15  2002/02/05 17:50:07  vltsccm
//   cdb1.15
//
//   Revision 1.14  2002/01/14 21:14:17  vltsccm
//   cdb1.14
//
//   Revision 1.13  2001/10/19 09:56:22  vltsccm
//   cdb1.13
//
//   Revision 1.12  2001/09/18 10:07:11  vltsccm
//   cdb1.12
//
//   Revision 1.11  2001/07/12 07:48:26  vltsccm
//   cdb1.11
//
//   Revision 1.10  2001/07/11 09:16:13  vltsccm
//   cdb1.10
//
//   Revision 1.6  2000/12/07 18:00:41  vltsccm
//   cdb1.6
//
//   Revision 1.5  2000/11/17 13:14:58  vltsccm
//   cdb1.5
//
//   Revision 1.4  2000/10/20 13:51:24  vltsccm
//   cdb1.4
//
//   Revision 1.3  2000/10/20 13:51:23  vltsccm
//   cdb1.3
//
//   Revision 1.2  2000/10/20 13:51:23  vltsccm
//   cdb1.2
//
//   Revision 1.1  2000/10/20 13:51:23  vltsccm
//   cdb1.1
//
//   Revision 1.0  2000/10/20 13:51:22  vltsccm
//   cdb1.0
//
//   Revision 1.3  2000/10/13 16:03:01  vltsccm
//   cdb1.3
//
//   Revision 1.2  2000/09/13 14:49:28  vltsccm
//   cdb1.2
//
//   Revision 1.1  2000/09/06 15:42:10  vltsccm
//   cdb1.1
//
//   Revision 1.1  2000/08/09 08:44:34  matej
//   1st working version of MACI
//
//   Revision 1.0  2000/08/08 16:25:57  matej
//   *** empty log message ***
//
//   Revision 1.2  2000/07/05 12:14:24  matej
//   *** empty log message ***
//
//   Revision 1.1  2000/06/13 07:26:25  kzagar
//   CDB, initial commit. Documentation not yet finished.
//
// ************************************************************************

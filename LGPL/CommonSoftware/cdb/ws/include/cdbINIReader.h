#ifndef cdbINIReader_H
#define cdbINIReader_H

/*******************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: cdbINIReader.h,v 1.25 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when       what
* --------  ---------- ----------------------------------------------
* msekoran  2001/05/09 created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acsutil.h"

#include "cdb.h"

#define INI_MAX_LINE_LENGTH 256
#define INI_SEPARATOR_CHAR '='
#define INI_BEGIN_SECTION_CHAR '['
#define INI_END_SECTION_CHAR ']'

namespace cdb {

class cdb_EXPORT INIReader : public Table
{

public:
  INIReader(const char * fileName);
  virtual ~INIReader();
	static  Table* createTable( int argc, char** argv, CORBA::ORB_ptr orb );

  virtual Boolean isInitialized();

  // WARNING: NextSection method is not thread-safe!!!
  Boolean NextSection(ACE_TCHAR * strRecordName, ACE_TCHAR * strFieldName, ACE_TCHAR * valueName);
  Boolean Rewind();

  // cdb::Table interface
  
  Boolean CreateRecord(const String &strRecordName,
                       Boolean bTruncate = FALSE);

  ULong GetRecordState(const String &strRecordName);

  Boolean GetField(const String &strRecordName,
                   const String &strFieldName,
                   Field &fld);

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

  Boolean GetChildren(const String &strRecordName,
		      StringArray &astrChildren);

private:

  /**
   * INI file handle
   */
  FILE * m_fileHandle;

  /**
   * The state of the object
   */
  Boolean m_initialized;

  /**
   * Current section, used by NextSection method
   */
  ACE_TCHAR m_section[INI_MAX_LINE_LENGTH];

};

 }; 

#endif /* cdbINIReader_H */

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: cdbINIReader.h,v $
// Revision 1.25  2006/09/01 02:20:54  cparedes
// small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
//
// Revision 1.24  2003/01/28 16:43:50  vltsccm
// gchiozzi: patch for cdb module to create lib/endorsed directory, since CVS cannot restore empty directories
//
// Revision 1.23  2003/01/24 10:44:05  vltsccm
// cdb1.23
//
// Revision 1.22  2003/01/20 15:12:20  vltsccm
// cdb1.22
//
// Revision 1.21  2003/01/20 10:45:53  vltsccm
// cdb1.21
//
// Revision 1.20  2002/12/05 16:03:59  vltsccm
// cdb1.20
//
// Revision 1.19  2002/11/25 16:04:50  vltsccm
// cdb1.19
//
// Revision 1.18  2002/11/13 14:53:05  vltsccm
// cdb1.18
//
// Revision 1.17  2002/11/13 10:22:31  vltsccm
// cdb1.17
//
// Revision 1.16  2002/11/06 08:37:05  vltsccm
// cdb1.16
//
// Revision 1.15.1.23  2002/11/05 16:05:14  vltsccm
// cdb1.15.1.23
//
// Revision 1.15.1.22  2002/11/05 13:46:31  vltsccm
// cdb1.15.1.22
//
// Revision 1.15.1.21  2002/11/05 10:41:14  vltsccm
// cdb1.15.1.21
//
// Revision 1.15.1.20  2002/11/01 12:49:03  vltsccm
// cdb1.15.1.20
//
// Revision 1.15.1.19  2002/10/30 07:56:44  vltsccm
// cdb1.15.1.19
//
// Revision 1.15.1.18  2002/10/25 12:44:24  vltsccm
// cdb1.15.1.18
//
// Revision 1.15.1.17  2002/10/24 13:08:44  vltsccm
// cdb1.15.1.17
//
// Revision 1.15.1.16  2002/10/16 11:43:45  vltsccm
// cdb1.15.1.16
//
// Revision 1.15.1.15  2002/10/14 22:26:11  vltsccm
// cdb1.15.1.15
//
// Revision 1.15.1.14  2002/10/14 12:18:33  vltsccm
// cdb1.15.1.14
//
// Revision 1.15.1.13  2002/10/04 16:20:24  vltsccm
// cdb1.15.1.13
//
// Revision 1.15.1.12  2002/10/02 12:54:15  vltsccm
// cdb1.15.1.12
//
// Revision 1.15.1.11  2002/10/01 10:33:25  vltsccm
// cdb1.15.1.11
//
// Revision 1.15.1.10  2002/09/30 13:56:52  vltsccm
// cdb1.15.1.10
//
// Revision 1.15.1.9  2002/09/26 14:13:11  vltsccm
// cdb1.15.1.9
//
// Revision 1.15.1.8  2002/09/26 07:45:47  vltsccm
// cdb1.15.1.8
//
// Revision 1.15.1.7  2002/09/17 16:19:23  vltsccm
// cdb1.15.1.7
//
// Revision 1.15.1.6  2002/09/17 11:15:48  vltsccm
// cdb1.15.1.6
//
// Revision 1.15.1.5  2002/09/02 09:37:07  vltsccm
// cdb1.15.1.5
//
// Revision 1.15.1.4  2002/08/09 09:35:24  vltsccm
// cdb1.15.1.4
//
// Revision 1.15.1.3  2002/07/24 07:29:11  vltsccm
// cdb1.15.1.3
//
// Revision 1.15.1.2  2002/07/12 09:58:18  vltsccm
// cdb1.15.1.2
//
// Revision 1.15+.1.1  2002/07/09 09:40:09  vltsccm
// cdb1.15.1
//
// Revision 1.15  2002/02/05 17:50:08  vltsccm
// cdb1.15
//
// Revision 1.14  2002/01/14 21:14:19  vltsccm
// cdb1.14
//
// Revision 1.13  2001/10/19 09:56:23  vltsccm
// cdb1.13
//
// Revision 1.12  2001/09/18 10:07:12  vltsccm
// cdb1.12
//
// Revision 1.11  2001/07/12 07:48:28  vltsccm
// cdb1.11
//
// Revision 1.10  2001/07/11 09:16:24  vltsccm
// cdb1.10
//
// Revision 1.9  2001/07/11 09:16:23  vltsccm
// cdb1.9
//
// Revision 1.8  2001/07/11 09:16:23  vltsccm
// cdb1.8
//
// Revision 1.7  2001/07/11 09:16:23  vltsccm
// cdb1.7
//
// Revision 1.6  2001/07/11 09:16:22  vltsccm
// cdb1.6
//
// Revision 1.5  2001/07/11 09:16:22  vltsccm
// cdb1.5
//
// Revision 1.4  2001/07/11 09:16:22  vltsccm
// cdb1.4
//
// Revision 1.3  2001/07/11 09:16:21  vltsccm
// cdb1.3
//
// Revision 1.2  2001/07/11 09:16:21  vltsccm
// cdb1.2
//
// Revision 1.1  2001/07/11 09:16:21  vltsccm
// cdb1.1
//
// Revision 1.0  2001/07/11 09:16:21  vltsccm
// cdb1.0
//
//
// ************************************************************************


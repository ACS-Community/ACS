#ifndef recoveryStore_H
#define recoveryStore_H

/*******************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: recoveryStore.h,v 1.9 2012/01/21 22:48:11 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran  18/02/01  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>
#include <recoveryExport.h>

#include <ace/pre.h>
#include <ace/SString.h>
#include <ace/Hash_Map_Manager.h>
#include <ace/Thread_Mutex.h>
#include <ace/Recursive_Thread_Mutex.h>

namespace recovery {

/**
 * Maximum record size (line in the recovery file)
 */
#define MAX_RECORD_SIZE 1500

/**
 * Separator which separates id from data in recovery file
 */
#define SEPARATOR_CHAR 1

/**
 * A container class for storing recovery information using ACE_CString class.
 * This implementation uses an ACE_Hash_Map_Manager internally to store 
 * recovery info. stored in ACE_CString. Permits fast searches by id.
 * It also supports saving data into a file.
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 * @version "@(#) $Id: recoveryStore.h,v 1.9 2012/01/21 22:48:11 tstaig Exp $"
 * @precondition keys must not contain SEPARATOR_CHAR --- 
 * this restriction should be somehow removed in implementation... 
 */

class recovery_EXPORT RecoveryStore
{
public:

 /**
  * Constructor
  * @param file_name file name of recovery file
  * @param load load existing recovery file
  * @param max_size maximum size of the store
  * @see set_max_size 
  */
  RecoveryStore (const ACE_CString& file_name, bool load = true, ACE_UINT32 max_size = 0);

 /**
  * Destructor
  */
  ~RecoveryStore (void);

 /**
  * Initialization method<br>
  * Calls ACE_Hash_Map open() method.
  * @return 0 if successful, -1 if failed
  */
  int open (void);

 /**
  * Finalization method<br>
  * Calls ACE_Hash_Map close() method.
  * @return 0 on success, -1 on failure
  */
  int close (void);

 /**
  * Get the current set value of the max size of the store.
  * @return current set value of max size
  */
  ACE_UINT32 get_max_size (void);

 /**
  * Set the max size of the store; size == 0 means infinite.
  * @param size max size of the store
  */
  void set_max_size (ACE_UINT32 size);

 /**
  * Gets the current size (in bytes) of the store (id+data).
  * @return current size (in bytes) of the store (id+data)
  */
  ACE_UINT32 get_current_size (void);

 /**
  * Get the number of records in the store right now.
  * @return number of records in the store
  */
  ACE_UINT32 get_n_records (void);

 /**
  * Insert rec into storage
  * @param id id of the record
  * @param data record data
  * @param bsave save data to recovery file after operation
  * @return 0 on success, -1 on failure, 1 if store if full
  */
  int log (const ACE_CString& id, const ACE_CString& data, bool bsave=true);

 /**
  * Checks if recordwith key <i>id</i> is already mapped
  * @param id id of the record
  * @return 0 if record with key <i>id</i> is mapped, otherwise -1
  */
  int exists (const ACE_CString& id);

 /**
  * Set <i>data</i> to the record data with the given id
  * @param id id of the record
  * @param data record data
  * @return 0 on success, -1 on failure
  */
  int retrieve (const ACE_CString& id, ACE_CString& data);

 /**
  * Update data in the storage to the record with the given id
  * @param id id of the record
  * @param data record data
  * @param bsave save data to recovery file after operation
  * @return 0 on success, -1 on failure, 1 if store if full
  */
  int update (const ACE_CString& id, const ACE_CString& data, bool bsave=true);

 /**
  * Remove record data with the given id from the storage
  * @param id id of the record
  * @param bsave save data to recovery file after operation
  * @return 0 on success, -1 on failure
  */
  int remove (const ACE_CString& id, bool bsave=true);

  typedef ACE_Hash_Map_Manager <ACE_CString, ACE_CString, ACE_Thread_Mutex> STORE_HASH_MAP;
  typedef ACE_Hash_Map_Iterator <ACE_CString, ACE_CString, ACE_Thread_Mutex> STORE_HASH_MAP_ITER;
  typedef ACE_Hash_Map_Entry <ACE_CString, ACE_CString> STORE_HASH_MAP_ENTRY;

 /**
  * Get iterator of the store
  * @return iterator of the store
  */
  STORE_HASH_MAP_ITER get_iterator (void);

 /**
  * Save records from the store to the recovery file
  * @return 0 on success, -1 on failure
  */
  int save(void); 

 /**
  * Load records from the recovery file to the store
  * @return 0 on success, -1 on failure
  */
  int load(void); 

 /**
  * Remove recovery store file
  * @return 0 on success, -1 on failure
  */
  int unlink(void); 

protected:

 /**
  * The ACE implementation of hash map (ACE_Hash_Map_Manager)
  */
  RecoveryStore::STORE_HASH_MAP rec_hash_;

 /**
  * Recovery store file name
  */
  ACE_CString file_name_;

 /**
  * The maximum size of the store.
  */
  ACE_UINT32 max_size_;

 /**
  * The current size (in bytes) of the store
  */
  ACE_UINT32 current_size_;

 /**
  * The current number of records in the store
  */
  ACE_UINT32 num_records_;

 /**
  * Thread mutex for thread-safe code
  */
  ACE_Recursive_Thread_Mutex mutex_;
};

#include <ace/post.h>

 }; 

#endif /* recoveryStore_H */

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: recoveryStore.h,v $
// Revision 1.9  2012/01/21 22:48:11  tstaig
// Backport from branches ACS-9_0_0-windows-B and ACS-9_1_0-windows-B to support
// ACS on Windows under Cygwin. This commit corresponds to several CommonSoftware
// modules.
//
// Revision 1.8  2006/09/01 02:20:55  cparedes
// small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
//
// Revision 1.7  2004/03/17 07:39:50  bjeram
// ported to ACE 5.4 and TAO 1.4
//
// Revision 1.6  2002/02/08 14:21:44  vltsccm
// bgustafs: Removed chmod for VxWorks
//
// Revision 1.5  2001/12/27 19:00:18  vltsccm
// recovery1.5
//
// Revision 1.4  2001/07/12 07:07:22  vltsccm
// recovery1.4
//
// Revision 1.3  2001/07/11 09:15:08  vltsccm
// recovery1.3
//
//
// ************************************************************************

/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: recoveryStore.cpp,v 1.7 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran  17/02/01  created 
*/

#include <vltPort.h>

#include <recoveryStore.h>

#include <logging.h>

ACE_RCSID(recovery, recoveryStore, "$Id: recoveryStore.cpp,v 1.7 2006/09/01 02:20:54 cparedes Exp $")

 using namespace recovery;

RecoveryStore::RecoveryStore (const ACE_CString& file_name, bool bload, ACE_UINT32 max_size)
  : file_name_(file_name),
    max_size_ (max_size),
    current_size_ (0),
    num_records_ (0)
{
  if (bload)
    load();
}

RecoveryStore::~RecoveryStore (void)
{
  unlink();
}

int
RecoveryStore::open (void)
{
  return rec_hash_.open ();
}

int
RecoveryStore::close (void)
{
  // Close the hash
  return rec_hash_.close ();
}

ACE_UINT32
RecoveryStore::get_max_size (void)
{
  return max_size_;
}

void
RecoveryStore::set_max_size (ACE_UINT32 size)
{
  max_size_ = size;
}

ACE_UINT32
RecoveryStore::get_current_size (void)
{
  return current_size_;
}

ACE_UINT32
RecoveryStore::get_n_records (void)
{
  return num_records_;
}

int
RecoveryStore::log (const ACE_CString& id, const ACE_CString& data, bool bsave)
{
  // Check if we are allowed to write...
  if (max_size_ !=0 && current_size_ >= max_size_)
    return 1; // return code for store if full

  // First, bind the id to the data in the hash_map
  if (rec_hash_.bind (id, data) != 0)
    return -1; // return code for failure

  // Increment the number of records in the store
  ++num_records_;
  current_size_ += sizeof (id) + sizeof (data);

  if (bsave) save();

  return 0;
}

int
RecoveryStore::retrieve(const ACE_CString& id, ACE_CString& data)
{
  int retval = rec_hash_.find (id, data);
  return retval;
}

int
RecoveryStore::exists(const ACE_CString& id)
{
  return rec_hash_.find (id);
}

int
RecoveryStore::update (const ACE_CString& id, const ACE_CString& data, bool bsave)
{
  if (rec_hash_.unbind (id) != 0)
    return -1; // return failure code

  int retval = rec_hash_.bind (id, data);
  
  if (bsave) save();

  return retval;
}

int
RecoveryStore::remove (const ACE_CString& id, bool bsave)
{
  ACE_CString data;
  if (rec_hash_.unbind (id, data) != 0)
    return -1; // return failure code

  --num_records_;
  current_size_ -= sizeof (id) + sizeof(data);

  if (bsave) save();

  return 0;
}

RecoveryStore::STORE_HASH_MAP_ITER
RecoveryStore::get_iterator (void) {
  return RecoveryStore::STORE_HASH_MAP_ITER(rec_hash_);
}

int
RecoveryStore::save(void)
{
#ifndef ACS_HAS_WIN32

  ACE_Guard<ACE_Recursive_Thread_Mutex> guard(mutex_);

  FILE *fp = ACE_OS::fopen (file_name_.c_str(), "w");

  if (fp==0)  {
      ACS_LOG(LM_RUNTIME_CONTEXT, "recovery::RecoveryStore::save", 
		  (LM_ERROR, "Unable to create recovery file (w) '%s'", file_name_.c_str()));
      return -1;
  }

  int cr_len = ACE_OS::strlen("\n"); 
  int result, to_write;
  result = to_write = 0;

  STORE_HASH_MAP_ENTRY *entry;
  for (STORE_HASH_MAP_ITER hash_iter (rec_hash_);
       (hash_iter.next (entry) != 0) && (result == to_write);
       hash_iter.advance ())
    {
      result = ACE_OS::fprintf(fp, "%s%c%s\n", entry->ext_id_.c_str(),
			       SEPARATOR_CHAR, entry->int_id_.c_str());

      to_write = entry->ext_id_.length()+
	entry->int_id_.length()+cr_len+1;  // 1 == space char. len 
    }

  ACE_OS::fclose (fp);

#ifndef ACS_HAS_WIN32
#ifndef MAKE_VXWORKS
  chmod(file_name_.c_str(), 0664);
#endif
#endif

  if (result!=to_write) {
      ACS_LOG(LM_RUNTIME_CONTEXT, "recovery::RecoveryStore::save", 
	       (LM_ERROR, "Failed to write recovery data to file '%s'",
	       file_name_.c_str()
	       ));
    return -1;
  }

#endif

  return 0;

}

int
RecoveryStore::load(void)
{
#ifndef ACS_HAS_WIN32

  ACE_Guard<ACE_Recursive_Thread_Mutex> guard(mutex_);

  FILE *fp = ACE_OS::fopen (file_name_.c_str(), "r");

  if (fp==0)  {
      ACS_LOG(LM_RUNTIME_CONTEXT, "recovery::RecoveryStore::load", 
			  (LM_DEBUG, "Unable to open recovery file (r) '%s'", file_name_.c_str()));
      return -1;
  }
  ACS_LOG(LM_RUNTIME_CONTEXT, "recovery::RecoveryStore::load", 
			(LM_INFO, "Reading recovery file '%s'", file_name_.c_str())); 

  int len;
  char* separator;
  ACE_TCHAR buffer[MAX_RECORD_SIZE];
  ACE_TCHAR* buf; 

  while ( (buf=fgets(buffer, MAX_RECORD_SIZE, fp)) != 0 ) {

    len = ACE_OS::strlen(buf);

    // find space character (id-data separator)
    separator = ACE_OS::strnchr (buf, SEPARATOR_CHAR, len);

    if ((separator == 0) || (len<2)) {     // at least 2 characters (space & cr)
	  ACS_LOG(LM_RUNTIME_CONTEXT, "recovery::RecoveryStore::load", 
			 (LM_INFO, "Corrupted recovery file '%s'", file_name_.c_str()));
      ACE_OS::fclose (fp);
      return -1;
    }

    // terminate strings
    *separator = 0; 
    buffer[len-1] = 0; 

    log(ACE_CString(buf), ACE_CString(++separator), false);

  }

  ACE_OS::fclose (fp);

#endif

  return 0;
}

int
RecoveryStore::unlink(void)
{
  ACE_Guard<ACE_Recursive_Thread_Mutex> guard(mutex_);
  return ACE_OS::unlink(file_name_.c_str());
}

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: recoveryStore.cpp,v $
// Revision 1.7  2006/09/01 02:20:54  cparedes
// small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
//
// Revision 1.6  2002/02/08 14:21:45  vltsccm
// bgustafs: Removed chmod for VxWorks
//
// Revision 1.5  2001/12/27 19:00:19  vltsccm
// recovery1.5
//
// Revision 1.4  2001/07/12 07:07:23  vltsccm
// recovery1.4
//
// Revision 1.3  2001/07/11 09:15:10  vltsccm
// recovery1.3
//
//
// ************************************************************************

/*___oOo___*/

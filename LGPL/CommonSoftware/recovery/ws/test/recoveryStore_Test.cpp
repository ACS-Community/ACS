/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: recoveryStore_Test.cpp,v 1.9 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran  17/02/01  created 
* acaproni  20/10/03  test integrated in the system
*/

#include <recoveryStore.h>
#include <logging.h>

ACE_RCSID(recovery, recoveryStore_test, "$Id: recoveryStore_Test.cpp,v 1.9 2006/09/01 02:20:54 cparedes Exp $")

 using namespace recovery;

struct String_Table {
  ACE_CString key_;
  ACE_CString value_;
};

static String_Table string_table[] =
{
  {
    ACE_CString ("hello"),
    ACE_CString ("guten Tag")
  },
  {
    ACE_CString ("goodbye"),
    ACE_CString ("auf wiedersehen")
  },
  {
    ACE_CString ("funny"),
    ACE_CString ("lustig")
  },
  {
    ACE_CString ("to sleep"),
    ACE_CString ("schlafen")
  },
  {
    ACE_CString ("to work"),
    ACE_CString ("nicht :-) arbeiten")
  },
  {
    ACE_CString (),
    ACE_CString ()
  }
};


int main() {

  RecoveryStore store("/tmp/recoveryStore_Test");
  //store.open();

  int i;

  // Let's test the iterator while we are at it.
  {
    RecoveryStore::STORE_HASH_MAP_ENTRY *entry;
    i = 0;
    for ( RecoveryStore::STORE_HASH_MAP_ITER hash_iter = store.get_iterator();
         hash_iter.next (entry) != 0;
         hash_iter.advance ())
      {
        ACS_SHORT_LOG ((LM_DEBUG,
                    ACE_TEXT ("iterating (%d): [%s, %s]"),
                    i,
                    entry->ext_id_.c_str(),
                    entry->int_id_.c_str()));
        i++;
      }
  }

  // Print the number of items found 
  ACS_SHORT_LOG ((LM_DEBUG,ACE_TEXT("%d items found"),i));

  // Check the <log> operation.
  ACS_SHORT_LOG((LM_DEBUG,ACE_TEXT("Checking <log>"))); 
  for (i = 0; string_table[i].key_.length() != 0; i++)
    if (store.log (string_table[i].key_,
                   string_table[i].value_) == -1)
	{
	ACS_SHORT_LOG ((LM_ERROR,
			ACE_TEXT ("failed to update for key %s"),
		       string_table[i].key_.c_str()));
	return -1;
	}

  
   if (store.log (string_table[2].key_,
                   string_table[2].value_) == 0)
       {
      ACS_SHORT_LOG((LM_ERROR,
                         ACE_TEXT ("it allowed duplicate keys for %s "),
                         string_table[i].key_.c_str()));
	return -1;
       }
   if (store.update (string_table[4].key_,
                   ACE_CString("arbeiten")) == -1)
       {
      ACS_SHORT_LOG((LM_ERROR,
                         ACE_TEXT ("it allowed duplicate keys for %s "),
                         string_table[i].key_.c_str()));
 	return -1;
      }
  ACE_CString entry;

  // Check the <find> operation.
  ACS_SHORT_LOG((LM_DEBUG,ACE_TEXT("Checking <find>"))); 
  for (i = 0; string_table[i].key_.length() != 0; i++)
    if (store.retrieve (string_table[i].key_,
                   entry) == 0) {
      ACS_SHORT_LOG ((LM_DEBUG,
                  ACE_TEXT ("`%s' found `%s'"),
                  string_table[i].key_.c_str(),
                  entry.c_str()));

      // Check if the string found is equals to the string inserted
      // (if the string found is arbeiten the check is not made beacuse we
      // changed that value without updating the array)
      ACS_SHORT_LOG((LM_DEBUG,ACE_TEXT("Key %s: found %s stored %s"), 
		string_table[i].key_.c_str(), 
		entry.c_str(),
		string_table[i].value_.c_str()));
      if (strcmp(entry.c_str(), string_table[i].value_.c_str())!=0 && strcmp( entry.c_str(),"arbeiten")!=0)
	      ACS_SHORT_LOG((LM_ERROR,ACE_TEXT("Error the strings differ: stored %s, found in list %s"),
			string_table[i].value_.c_str(),
			entry.c_str()));
    } else
	{
	ACS_SHORT_LOG ((LM_ERROR,
			ACE_TEXT ("`%s' not found"),
			string_table[i].key_.c_str()));
	return -1;
	}
   if (store.retrieve (ACE_CString("no such key"),
                   string_table[0].value_) == 0)
       {
       ACS_SHORT_LOG ((LM_ERROR,
                         ACE_TEXT ("reports it had found nonexisting key; value: %s "),
                         string_table[i].key_.c_str()));
       return -1;
       }


  // Let's test the iterator while we are at it.
  ACS_SHORT_LOG((LM_DEBUG,ACE_TEXT("Checking iterator"))); 
  {
    RecoveryStore::STORE_HASH_MAP_ENTRY *entry;
    i = 0;
    for ( RecoveryStore::STORE_HASH_MAP_ITER hash_iter = store.get_iterator();
         hash_iter.next (entry) != 0;
         hash_iter.advance ())
      {
        ACS_SHORT_LOG ((LM_DEBUG,
                    ACE_TEXT ("iterating (%d): [%s, %s]"),
                    i,
                    entry->ext_id_.c_str(),
                    entry->int_id_.c_str()));
        i++;
      }
  }

  // Check <remove>
  ACS_SHORT_LOG((LM_DEBUG,ACE_TEXT("Checking <remove>"))); 
  for (i = 0; string_table[i].key_.length() != 0; i++)
    if (store.remove (string_table[i].key_) == -1)
	{
	ACS_SHORT_LOG((LM_ERROR,
		       ACE_TEXT ("failed to remove for key %s"),
		       string_table[i].key_.c_str()))
	    return -1;
	}
  // Let's test the iterator while we are at it.
  {
    RecoveryStore::STORE_HASH_MAP_ENTRY *entry;
    i = 0;
    for ( RecoveryStore::STORE_HASH_MAP_ITER hash_iter = store.get_iterator();
         hash_iter.next (entry) != 0;
         hash_iter.advance ())
      {
        ACS_SHORT_LOG ((LM_DEBUG,
                    ACE_TEXT ("iterating (%d): [%s, %s]"),
                    i,
                    entry->ext_id_.c_str(),
                    entry->int_id_.c_str()));
        i++;
      }
  }

  store.close();

  return 0;
}
/*___oOo___*/

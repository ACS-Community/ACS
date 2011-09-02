#ifndef baciRecovery_h
#define baciRecovery_h

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
* "@(#) $Id: baciRecovery.h,v 1.101 2011/09/02 11:39:00 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/02/20  completely modified
*/

/** 
 * @file 
 * Header file for BACI Recovery.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>
#include <baciExport.h>

#include <recoveryStore.h>
#include <baciRecoverableObject.h>

#include <ace/SString.h>
#include <vector>

namespace baci {

/**
 * Default file name of BACI recovery file
 */
#define BACI_RECOVERY_FILE_NAME "baci_recovery"

/**
 * Maximum IOR length
 */
#define MAX_IOR_LENGTH 800

/**
 * Maximum object name length
 */
#define MAX_NAME_LENGTH 50

/**
 * Class which saves all BACI data needed to restore 
 * its state in cause of failure or unexpected 
 * terminatrion. <br>
 * It uses RecoveryStore class from recovery module to store
 * data. Since all data are coded to a ACE_CString, this class
 * also provided data coding/decoding. There are only two structures
 * to save, actually: monitors and events.
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 * @see RecoverableObject 
 */

class baci_EXPORT BACIRecoveryManager {

public:

  /**
   * Constructor
   */
  BACIRecoveryManager();
  
  /**
   * Destructor
   */
  ~BACIRecoveryManager();

  /**
   * Set load recovery data switch
   * @param bool load
   */
  static void loadRecovery(bool load);

  /**
   * Set activator name.
   * @param activator name
   */
  static void activatorName(const char * activatorName);

  /**
   * Get static instance of BACIRecoveryManager ("singleton" pattern)
   * @return static instance
   */
  static BACIRecoveryManager* getInstance(void);
 
  /**
   * Destroy static instance of BACIRecoveryManager (clean-up/delete recovery data)
   */
  static void destroyInstance();

  /**
   * Add object to recovery data
   * @param object object to be added to recovery data
   */
  void addRecoverableObject(RecoverableObject *object);

  /**
   * Update object to recovery data
   * @param object object to be updated in recovery data
   */
  void updateRecoverableObject(RecoverableObject *object);

  /**
   * Remove object from recovery data
   * @param object object to be removed from recovery data
   */
  void removeRecoverableObject(RecoverableObject *object);

  /**
   * Get objects' names starting with given prefix
   * @param namePrefix name prefix
   */
  ACE_CString_Vector getObjectsStartingWith(const char* namePrefix);

  /**
   * Get object's stringified state
   * @param name object name
   */
  const char* getObjectState(const char* name);

  /**
   * Generates first free name starting with given prefix <br>
   * Generation algorithm: <i>namePrefix, namePrefix0, namePrefix1, etc. </i>
   * @param namePrefix name prefix
   */
   char* generateObjectName(const char* namePrefix);

private:
  
  /**
   * Class which takes care of recovery data
   * @see RecoveryStore
   */
  recovery::RecoveryStore* store_mp;

  /**
   * Static instance of BACIRecoveryManager
   */                        
  static BACIRecoveryManager* instance_mp;

  /**
   * Load recovery data switch
   */                        
  static bool load_m;

  /**
   * Activator name
   */
  static const char * activatorName_mp;
    
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const BACIRecoveryManager&);
    

    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    BACIRecoveryManager(const BACIRecoveryManager&);  
};

 }; 

#endif /* baciRecovery_h */







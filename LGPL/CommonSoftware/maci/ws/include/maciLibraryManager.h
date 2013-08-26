#ifndef maciLibraryManager_h
#define maciLibraryManager_h

/*******************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciLibraryManager.h,v 1.83 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/05/19  redesigned
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>

#include <tao/corba.h>
#include <tao/PortableServer/PortableServer.h>

#include <maciRegistrar.h>

#include <acsutilThreadInit.h>
#include <ace/SString.h>
#include <ace/Synch.h>

#include <logging.h>

namespace maci {

/**
 * Class responsible of loading/unloading of dynamic-linked/shared libraries
 */
class LibraryManager
{

public:

  /**
   * Declaration of the DLLOpen function.
   * A DLL that expects to be loaded by the DLL Manager can export a
   * function named "DLLOpen". The DLL Manager then invokes this function
   * just after it loads the DLL.
   * @param argc number of arguments
   * @param argv array of arguments
   * @return false on initialization failure (false indicates that DLL will be unloaded).
   */  
  typedef bool (*DLLOpenFunc)(int argc, char *argv[]);


  /**
   * Declaration of the DLLClose function.
   * A DLL that expects to be unloaded by the DLL Manager can export a
   * function named "DLLClose". The DLL Manager then invokes this function
   * just before it unloads the DLL.
   */  
  typedef void (*DLLCloseFunc)(void);


  /**
   * Declaration of the DLLSetupCORBA function.
   * A DLL that expects to be loaded by the DLL Manager can export a
   * function named "DLLSetupCORBA". The DLL Manager then invokes this function
   * just after it loads the DLL.
   * @param m_orb
   * @param m_poaRoot
   * @param m_poaPersistent
   */  
  typedef void (*DLLSetupCORBAFunc)(CORBA::ORB_ptr m_orb,
				    PortableServer::POAManager_ptr m_poaManager,
				    PortableServer::POA_ptr m_poaRoot,
				    PortableServer::POA_ptr m_poaPersistent,
				    PortableServer::POA_ptr m_poaTransient);
  
  /**
   * Declaration of the DLLInitThreads function.
   * A DLL that expects to be loaded by the DLL Manager can export a
   * function named "DLLInitThreads". The DLL Manager then invokes this function
   * just after it loads the DLL.
   * @param m_orb
   * @param m_poaRoot
   * @param m_poaPersistent
   */  
  typedef void (*DLLInitThreadsFunc)(InitThreadFunc initFunc, DoneThreadFunc doneFunc);

  /**
   * Constructor
   */
  LibraryManager();

  /**
   * Destructor
   */
  ~LibraryManager();

  /**
   * Set path where to libraries can be found
   * @param path
   */
  void setSearchPath(const char * path);

  /**
   * Get path where to libraries can be found
   * @param path
   */
  const char * getSearchPath() { return m_path; }

  /**
   * Close all currently opened libraries
   * @param inform if true DLLClose function will be called
   */
  void closeAllLibraries(bool inform = true);

  /**
   * Get path of given library
   * (e.g. ps returns /usr/lib/libps.so)
   * @param name "core" name of the library
   */
  char * locateLibrary(const char * name);


  /**
   * Lock library (increase reference counter)
   * @param handle given by open method
   * @see open
   */
  void lock(int h);

  /**
   * Unlock library (decrease reference counter)
   * @param handle given by open method
   * @see open
   */
  void unlock(int h);

  /**
   * Loads library
   * @param path
   * @param argc
   * @param argv
   */
  int load(const char * path, int argc = 0, char *argv[] = 0);

  /**
   * Forces to unload the library (ignoring existing references to this library)
   */
  void unload(int h);

/**
   * Forces to unload the library (ignoring existing references to this library)
   */
  void unload(const char* name);

  /**
   * Resolves symbol from the library
   * @param handle given by open method
   * @see open
   */
  void * getSymbol(int h, const char * symbol);

  /**
   * Calls "DLLSetupCORBA" in library with specified handle
   * @param handle
   * @param m_orb
   * @param m_poaRoot
   * @param m_poaPersistent
   * @return true if successful
   */
  bool setupCORBAinDLL(int handle,
		       CORBA::ORB_ptr m_orb,
		       PortableServer::POAManager_ptr m_poaManager,
		       PortableServer::POA_ptr m_poaRoot,
		       PortableServer::POA_ptr m_poaPersistent,
		       PortableServer::POA_ptr m_poaTransient);

  /**
   * Calls "DLLInitThreads" in library with specified handle
   * @param handle
   * @param initFunc pointer to function
   * @param doneFunc pointer to function
   * @return true if successful
   */
  bool initThreadsInDLL(int handle, InitThreadFunc initFunc, DoneThreadFunc doneFunc);

private:

  /// Structure to store data about loaded libraries
  struct LibraryInfo
  {
    /// system library handle
    ACE_SHLIB_HANDLE hLib;
    /// reference(s) to the library
    int nRefCount;
    /// full path to the library 
    ACE_CString path;
  };

  /// Data about all loadrd libraries
  Registrar<int, LibraryInfo> m_libraries;
  
  /// Search path
  char * m_path;

  /**
   * Search throuh search path(s) for library with given name
   * @param filename library name
   * @param pathname found path to the library
   * @return 0 on success, -1 on failure
   */
  int ldfind(const ACE_TCHAR filename[],
	     ACE_TCHAR pathname[],
	     size_t maxpathnamelen);

  /// Thread synchornization mutex
  ACE_Recursive_Thread_Mutex mutex;

    /**
     * Returns an ACS Logger created for this container.
     * @return an ACS Logger
     */
    Logging::Logger::LoggerSmartPtr
    getLogger() {return logger_m;}

    /// Logger for this container;
    Logging::Logger::LoggerSmartPtr logger_m;

};

 }; 

#endif // maciLibraryManager_h


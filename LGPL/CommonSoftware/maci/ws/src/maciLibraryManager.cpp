/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciLibraryManager.cpp,v 1.93 2011/09/02 11:00:19 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bgustafs  2002-01-16  do not load a VxWorks library if already loaded
* msekoran  2001/02/21  created 
*/

#include <vltPort.h>

#include <maciLibraryManager.h>

#include <logging.h>

ACE_RCSID(maci, maciLibraryManager, "$Id: maciLibraryManager.cpp,v 1.93 2011/09/02 11:00:19 bjeram Exp $");

namespace maci {

#ifdef MAKE_VXWORKS
# include "VxWorksDLL.cpp"
#endif

LibraryManager::LibraryManager() : 
    m_path(0)
{
    ACS_CHECK_LOGGER;
    logger_m = getNamedLogger("maci::LibraryManager");
    ACS_TRACE("maci::LibraryManager::LibraryManager");
}

LibraryManager::~LibraryManager()
{
  ACS_TRACE("maci::LibraryManager::~LibraryManager");
  if (m_path)
      { delete[] m_path; m_path = 0; }
}

void
LibraryManager::setSearchPath(const char * path)
{
  ACE_Guard<ACE_Recursive_Thread_Mutex> guard(mutex);

  ACS_TRACE("maci::LibraryManager::setSearchPath");

  if (m_path)
      { delete[] m_path; m_path = 0; }

  m_path = new ACE_TCHAR[ACE_OS::strlen(path)+1];
  ACE_OS::strcpy(m_path, path);

  for (unsigned int pos = 0; m_path[pos]; pos++)
#if ACE_DIRECTORY_SEPARATOR_CHAR == '\\'
    if (m_path[pos]=='/')
#else
      if (m_path[pos]=='\\')
#endif
	m_path[pos] = ACE_DIRECTORY_SEPARATOR_CHAR;

}

void LibraryManager::closeAllLibraries(bool inform)
{

  ACE_Guard<ACE_Recursive_Thread_Mutex> guard(mutex);

  ACS_TRACE("maci::LibraryManager::closeAllLibraries");

  for(int h = m_libraries.Last(); h!=0;)
    {
      if(inform)
	{
	  ACS_DEBUG_PARAM("maci::LibraryManager::closeAllLibraries", 
			  "Forcing unload DLL '%s'", m_libraries[h].path.c_str());
    
#ifndef MAKE_VXWORKS
	  DLLCloseFunc dllClose = (DLLCloseFunc)ACE_OS::dlsym(m_libraries[h].hLib, "DLLClose");
#else
	  DLLCloseFunc dllClose = (DLLCloseFunc)vxworks_dlsym(m_libraries[h].hLib, "DLLClose");
#endif
	  if (dllClose!=0)
	    {
	      ACS_DEBUG("maci::LibraryManager::closeAllLibraries", "Executing DLLClose");
	      dllClose();
	      ACS_DEBUG("maci::LibraryManager::closeAllLibraries", "DLLClose executed");
	    }
	}
    
#ifndef MAKE_VXWORKS                  
      ACE_OS::dlclose(m_libraries[h].hLib);
#else
      vxworks_dlclose(m_libraries[h].hLib);
#endif
    
      int t = h;
      h = m_libraries.Previous(h);
      m_libraries.Deallocate(t);
    
    }
  
}


//
// The following code is copied from ACE.cpp and is a courtesy of DOC
// group, Washington University, St. Luis.
//
// - Call to access() has been replaced with a call to fopen(..., "r"),
// because access does not exist on VxWorks.
//
// - search path is obtained from m_path and not via getenv
//
int
LibraryManager::ldfind(const ACE_TCHAR filename[],
		       ACE_TCHAR pathname[],
		       size_t maxpathnamelen)
{
  ACS_TRACE ("maci::LibraryManager::ldfind");
  
  FILE *fh;

#if defined (ACE_WIN32) && !defined (ACE_HAS_WINCE) && \
    !defined (ACE_HAS_PHARLAP)
  ACE_TCHAR expanded_filename[MAXPATHLEN];
#if !defined (ACE_HAS_MOSTLY_UNICODE_APIS)
  if (::ExpandEnvironmentStringsA (filename,
                                   expanded_filename,
                                   sizeof expanded_filename))
#else
    if (::ExpandEnvironmentStringsW (filename,
				     expanded_filename,
				     sizeof expanded_filename))
#endif /* ACE_HAS_MOSTLY_UNICODE_APIS */
      filename = expanded_filename;
#endif /* ACE_WIN32 && !ACE_HAS_WINCE && !ACE_HAS_PHARLAP */

  ACE_TCHAR tempcopy[MAXPATHLEN + 1];
  ACE_TCHAR searchpathname[MAXPATHLEN + 1];
  ACE_TCHAR searchfilename[MAXPATHLEN + 2];

  // Create a copy of filename to work with.
  if (ACE_OS::strlen (filename) + 1
      > (sizeof tempcopy / sizeof (ACE_TCHAR)))
    {
      errno = ENOMEM;
      return -1;
    }
  else
    ACE_OS::strcpy (tempcopy, filename);

  // Insert canonical directory separators.
  ACE_TCHAR *separator_ptr;

#if (ACE_DIRECTORY_SEPARATOR_CHAR != '/')
  // Make all the directory separators ``canonical'' to simplify
  // subsequent code.
  ACE::strrepl (tempcopy, ACE_DIRECTORY_SEPARATOR_CHAR, '/');
#endif /* ACE_DIRECTORY_SEPARATOR_CHAR */

  // Separate filename from pathname.
  separator_ptr = ACE_OS::strrchr (tempcopy, '/');

  // This is a relative path.
  if (separator_ptr == 0)
    {
      searchpathname[0] = '\0';
      ACE_OS::strcpy (searchfilename, tempcopy);
    }
  else // This is an absolute path.
    {
      ACE_OS::strcpy (searchfilename, separator_ptr + 1);
      separator_ptr[1] = '\0';
      ACE_OS::strcpy (searchpathname, tempcopy);
    }


  int got_suffix = 0;

  // Check to see if this has an appropriate DLL suffix for the OS
  // platform.
  ACE_TCHAR *s = ACE_OS::strrchr (searchfilename, '.');

  const ACE_TCHAR *dll_suffix =
    ACE_TEXT (ACE_DLL_SUFFIX);

  if (s != 0)
    {
      // If we have a dot, we have a suffix
      got_suffix = 1;

      // Check whether this matches the appropriate platform-specific
      // suffix.
      if (ACE_OS::strcmp (s, dll_suffix) != 0)
        {
          ACS_SHORT_LOG ((LM_WARNING,
                      ACE_TEXT ("Warning: improper suffix for a ")
                      ACE_TEXT ("shared library on this platform: %s"),
                      s));
        }
    }

  // Make sure we've got enough space in searchfilename.
  if (ACE_OS::strlen (searchfilename)
      + ACE_OS::strlen (ACE_DLL_PREFIX)
      + got_suffix ? 0 : ACE_OS::strlen (dll_suffix) >= (sizeof searchfilename /
                                                         sizeof (ACE_TCHAR)))
    {
      errno = ENOMEM;
      return -1;
    }


#if defined (ACE_WIN32) && defined (_DEBUG) && !defined (ACE_DISABLE_DEBUG_DLL_CHECK)
  size_t len_searchfilename = ACE_OS::strlen (searchfilename);
  if (! got_suffix)
    {
      searchfilename [len_searchfilename] = 'd';
      searchfilename [len_searchfilename+1] = 0;
    }

  // first search with searchfilename + 'd', then without
  for (int tag = 1; tag >= 0; tag --)
    {
      if (tag == 0)
        searchfilename [len_searchfilename] = 0;

#endif /* ACE_WIN32 && _DEBUG && !ACE_DISABLE_DEBUG_DLL_CHECK */

      // Use absolute pathname if there is one.
      if (ACE_OS::strlen (searchpathname) > 0)
	{
	  if (ACE_OS::strlen (searchfilename)
	      + ACE_OS::strlen (searchpathname) >= maxpathnamelen)
	    {
	      errno = ENOMEM;
	      return -1;
	    }
	  else
	    {
#if (ACE_DIRECTORY_SEPARATOR_CHAR != '/')
	      // Revert to native path name separators.
	      ACE::strrepl (searchpathname,
			    '/',
			    ACE_DIRECTORY_SEPARATOR_CHAR);
#endif /* ACE_DIRECTORY_SEPARATOR_CHAR */

	      // First, try matching the filename *without* adding a
	      // prefix.
#if defined (ACE_HAS_BROKEN_CONDITIONAL_STRING_CASTS)
	      ACE_OS::sprintf (pathname,
			       ACE_TEXT ("%s%s%s"),
			       searchpathname,
			       searchfilename,
			       got_suffix ? ACE_static_cast (char *,
							     ACE_TEXT (""))
			       : ACE_static_cast (char *,
						  dll_suffix));
#else /* ! defined (ACE_HAS_BROKEN_CONDITIONAL_STRING_CASTS) */
	      ACE_OS::sprintf (pathname,
			       ACE_TEXT ("%s%s%s"),
			       searchpathname,
			       searchfilename,
			       got_suffix ? ACE_TEXT ("") : dll_suffix);
#endif /* ! defined (ACE_HAS_BROKEN_CONDITIONAL_STRING_CASTS) */

	      if((fh = fopen(pathname, "r")) != 0)
		{
		  fclose(fh);
		  return 0;
		}
	      //if (ACE_OS::access (pathname, F_OK) == 0)
	      //  return 0;

	      // Second, try matching the filename *with* adding a prefix.
#if defined (ACE_HAS_BROKEN_CONDITIONAL_STRING_CASTS)
	      ACE_OS::sprintf (pathname,
			       ACE_TEXT ("%s%s%s%s"),
			       searchpathname,
			       ACE_DLL_PREFIX,
			       searchfilename,
			       got_suffix ? ACE_static_cast (char *,
							     ACE_TEXT (""))
			       : ACE_static_cast (char *,
						  dll_suffix));
#else /* ! defined (ACE_HAS_BROKEN_CONDITIONAL_STRING_CASTS) */
	      ACE_OS::sprintf (pathname,
			       ACE_TEXT ("%s%s%s%s"),
			       searchpathname,
			       ACE_DLL_PREFIX,
			       searchfilename,
			       got_suffix ? ACE_TEXT ("") : dll_suffix);
#endif /* ! defined (ACE_HAS_BROKEN_CONDITIONAL_STRING_CASTS) */
	      //if (ACE_OS::access (pathname, F_OK) == 0)
	      //  return 0;
	      if((fh = fopen(pathname, "r")) != 0)
		{
		  fclose(fh);
		  return 0;
		}
	    }
	}

      // Use relative filenames via LD_LIBRARY_PATH or PATH (depending on
      // OS platform).
      else
	{
	  char ld_path_buf[2048];
	  char *ld_path = ld_path_buf;
	  ACE_OS::strncpy(ld_path, m_path, sizeof(ld_path_buf));

#if defined (ACE_WIN32)
	  char *ld_path_temp = 0;
	  if (ld_path != 0)
	    {
	      ld_path_temp = (char *) ACE_OS::malloc (ACE_OS::strlen (ld_path) + 2);
	      if (ld_path_temp != 0)
		{
		  ACE_OS::strcpy (ld_path_temp, ACE_LD_SEARCH_PATH_SEPARATOR_STR);
		  ACE_OS::strcat (ld_path_temp, ld_path);
		  ld_path = ld_path_temp;
		}
	      else
		{
		  ACE_OS::free ((void *) ld_path_temp);
		  ld_path = ld_path_temp = 0;
		}
	    }
#endif /* ACE_WIN32 */

	  if (ld_path != 0
	      && (ld_path = ACE_OS::strdup (ld_path)) != 0)
	    {
	      // strtok has the strange behavior of not separating the
	      // string ":/foo:/bar" into THREE tokens.  One would expect
	      // that the first iteration the token would be an empty
	      // string, the second iteration would be "/foo", and the
	      // third iteration would be "/bar".  However, this is not
	      // the case; one only gets two iterations: "/foo" followed
	      // by "/bar".

	      // This is especially a problem in parsing Unix paths
	      // because it is permissible to specify 'the current
	      // directory' as an empty entry.  So, we introduce the
	      // following special code to cope with this:

	      // Look at each dynamic lib directory in the search path.

	      char *nextholder = 0;
	      const char *path_entry =
		ACE::strsplit_r (ld_path,
				 ACE_LD_SEARCH_PATH_SEPARATOR_STR,
				 nextholder);
	      int result = 0;

	      for (;;)
		{
		  // Check if at end of search path.
		  if (path_entry == 0)
		    {
		      errno = ENOENT;
		      result = -1;
		      break;
		    }
		  else if (ACE_OS::strlen (path_entry)
			   + 1
			   + ACE_OS::strlen (searchfilename)
			   >= maxpathnamelen)
		    {
		      errno = ENOMEM;
		      result = -1;
		      break;
		    }
		  // This works around the issue where a path might have
		  // an empty component indicating 'current directory'.
		  // We need to do it here rather than anywhere else so
		  // that the loop condition will still work.
		  else if (path_entry[0] == '\0')
		    path_entry = ".";

		  // First, try matching the filename *without* adding a
		  // prefix.
#if defined (ACE_HAS_BROKEN_CONDITIONAL_STRING_CASTS)
		  ACE_OS::sprintf (pathname,
				   ACE_TEXT ("%s%c%s%s"),
				   path_entry,
				   ACE_DIRECTORY_SEPARATOR_CHAR,
				   searchfilename,
				   got_suffix ? ACE_static_cast (char *,
								 ACE_TEXT (""))
				   : ACE_static_cast (char *,
						      dll_suffix));
#else /* ! defined (ACE_HAS_BROKEN_CONDITIONAL_STRING_CASTS) */
#ifndef MAKE_VXWORKS
		  ACE_OS::sprintf (pathname,
				   ACE_TEXT ("%s%c%s%s"),
				   path_entry,
				   ACE_DIRECTORY_SEPARATOR_CHAR,
				   searchfilename,
				   got_suffix ? ACE_TEXT ("") : dll_suffix);
#else
		  ACE_OS::sprintf (pathname,
				   ACE_TEXT ("%s%c%s"),
				   path_entry,
				   ACE_DIRECTORY_SEPARATOR_CHAR,
				   searchfilename);
#endif
#endif /* ! defined (ACE_HAS_BROKEN_CONDITIONAL_STRING_CASTS) */

		  if((fh = fopen(pathname, "r")) != 0)
		    {
		      fclose(fh);
		      break;
		    }            
                
		  //if (ACE_OS::access (pathname, F_OK) == 0)
		  //  break;

		  // Second, try matching the filename *with* adding a
		  // prefix.
#if defined (ACE_HAS_BROKEN_CONDITIONAL_STRING_CASTS)
		  ACE_OS::sprintf (pathname,
				   ACE_TEXT ("%s%c%s%s%s"),
				   path_entry,
				   ACE_DIRECTORY_SEPARATOR_CHAR,
				   ACE_DLL_PREFIX,
				   searchfilename,
				   got_suffix ? ACE_static_cast (char *,
								 ACE_TEXT (""))
				   : ACE_static_cast (char *,
						      dll_suffix));
#else /* ! defined (ACE_HAS_BROKEN_CONDITIONAL_STRING_CASTS) */
		  ACE_OS::sprintf (pathname,
				   ACE_TEXT ("%s%c%s%s%s"),
				   path_entry,
				   ACE_DIRECTORY_SEPARATOR_CHAR,
				   ACE_DLL_PREFIX,
				   searchfilename,
				   got_suffix ? ACE_TEXT ("") : dll_suffix);
#endif /* ! defined (ACE_HAS_BROKEN_CONDITIONAL_STRING_CASTS) */

		  if((fh = fopen(pathname, "r")) != 0)
		    {
		      fclose(fh);
		      break;
		    }

		  //if (ACE_OS::access (pathname, F_OK) == 0)
		  //  break;

		  // Fetch the next item in the path
		  path_entry = ACE::strsplit_r (0,
						ACE_LD_SEARCH_PATH_SEPARATOR_STR,
						nextholder);
		}

#if defined (ACE_WIN32)
	      if (ld_path_temp != 0)
		ACE_OS::free (ld_path_temp);
#endif /* ACE_WIN32 */
	      ACE_OS::free ((void *) ld_path);
#if defined (ACE_WIN32) && defined (_DEBUG) && !defined (ACE_DISABLE_DEBUG_DLL_CHECK)
	      if (result == 0 || tag == 0)
#endif /* ACE_WIN32 && _DEBUG && !ACE_DISABLE_DEBUG_DLL_CHECK */
		return result;
	    }
	}
#if defined (ACE_WIN32) && defined (_DEBUG) && !defined (ACE_DISABLE_DEBUG_DLL_CHECK)
    }
#endif /* ACE_WIN32 && _DEBUG && !ACE_DISABLE_DEBUG_DLL_CHECK */

  errno = ENOENT;
  return -1;
}



char *
LibraryManager::locateLibrary(const char * name)
{
  ACS_TRACE("maci::LibraryManager::locateLibrary");

  ACE_TCHAR * szPathName = new ACE_TCHAR[MAXPATHLEN+1];
  if(ldfind(name, szPathName, MAXPATHLEN+1) == 0)
    return szPathName;
  else
    return 0;
}

void
LibraryManager::lock(int h)
{
  ACS_TRACE("maci::LibraryManager::lock");
  if (m_libraries.IsAllocated(h))
    m_libraries[h].nRefCount++;
}

void
LibraryManager::unlock(int h)
{
  ACS_TRACE("maci::LibraryManager::unlock");
  if (m_libraries.IsAllocated(h))
    if (--m_libraries[h].nRefCount==0) 
      unload(h);
}

int
LibraryManager::load(const char * name, int argc, char *argv[])
{

  ACE_Guard<ACE_Recursive_Thread_Mutex> guard(mutex);

  ACS_TRACE("maci::LibraryManager::load");

  ACS_LOG(0, "maci::LibraryManager::load", (LM_INFO, "Request to load '%s'.", name));

  char * path = locateLibrary(name);

  if(!path)
    {
      ACS_LOG(0, "maci::LibraryManager::load", (LM_INFO, "Library '%s' not found.", name));
      return 0;
    }

  ACS_LOG(0, "maci::LibraryManager::load", (LM_INFO, "Full path '%s'", path));

  int i;
  for (i = m_libraries.First(); i!=0; i = m_libraries.Next(i))
    if (ACE_OS::strcmp(m_libraries[i].path.c_str(), path)==0)
      {
	++m_libraries[i].nRefCount;
	delete[] path;
	return i;
      }
    
  ACS_DEBUG_PARAM("maci::LibraryManager::load", "Loading '%s'.", path);

#ifdef ACS_HAS_WIN32
  ACE_SHLIB_HANDLE handle = ACE_OS::dlopen(path); 
  if (handle == ACE_SHLIB_INVALID_HANDLE)
    {
      ACS_LOG(LM_RUNTIME_CONTEXT, "maci::LibraryManager::load", (LM_ERROR, "Cannot load '%s'", path));
      delete[] path;
      return 0;
    }
#elif !defined(MAKE_VXWORKS)
  const char* errorstr;
  
  /*
    ACE_SHLIB_HANDLE handle = ACE_OS::dlopen(path); 
    if (handle == ACE_SHLIB_INVALID_HANDLE)
    {
    errorstr = dlerror();
    if (!errorstr) errorstr = "unknown";
    ACS_LOG(LM_RUNTIME_CONTEXT, "maci::LibraryManager::load",
    (LM_ERROR, Cannot load '%s', error: %s", path, errorstr));
    delete[] path;
    return 0;
    }
  */

  ACE_SHLIB_HANDLE handle = dlopen (path, RTLD_GLOBAL | RTLD_LAZY);
  if (!handle) 
    {
      errorstr = ACE_OS::dlerror();
      if (!errorstr) errorstr = "unknown";
      ACS_LOG(LM_RUNTIME_CONTEXT, "maci::LibraryManager::load", (LM_ERROR, "Cannot load '%s', error: %s", path, errorstr));
      delete[] path;
      return 0;
    }
  
#else
  ACE_SHLIB_HANDLE handle = vxworks_modulefind(const_cast<char*>(name));
  if (handle == ACE_SHLIB_INVALID_HANDLE)
    {
    handle = vxworks_dlopen(path); 
    if (handle == ACE_SHLIB_INVALID_HANDLE)
      {
        ACS_LOG(LM_RUNTIME_CONTEXT, "maci::LibraryManager::load", (LM_ERROR, "Cannot load '%s'", path));
        delete[] path;
        return 0;
      }
    }
#endif 


#ifndef MAKE_VXWORKS
  DLLOpenFunc dllOpen = (DLLOpenFunc)ACE_OS::dlsym(handle, "DLLOpen");
#else
  DLLOpenFunc dllOpen = (DLLOpenFunc)vxworks_dlsym(handle, "DLLOpen");
#endif  

  if (dllOpen!=0)
    {
      ACS_DEBUG("maci::LibraryManager::load", "Executing DLLOpen");
      if (!dllOpen(argc, argv))
	{
	  ACS_DEBUG("maci::LibraryManager::load", "Initialization failed, closing DLL");
#ifndef MAKE_VXWORKS
	  ACE_OS::dlclose(handle);
#else
	  vxworks_dlclose(handle);
#endif
	  delete[] path;
	  return 0;
	}
      ACS_DEBUG("maci::LibraryManager::load", "DLLOpen executed.");
    }

  i = m_libraries.Allocate();
  m_libraries[i].hLib = handle;
  m_libraries[i].nRefCount = 1;
  m_libraries[i].path = path;

  ACS_LOG(0, "maci::LibraryManager::load", (LM_INFO, "Loaded '%s'.", path));

  delete[] path;

  return i;
}

bool
LibraryManager::setupCORBAinDLL(int i, 
				CORBA::ORB_ptr m_orb,
				PortableServer::POAManager_ptr m_poaManager,
				PortableServer::POA_ptr m_poaRoot,
				PortableServer::POA_ptr m_poaPersistent,
				PortableServer::POA_ptr m_poaTransient)
{

  ACE_Guard<ACE_Recursive_Thread_Mutex> guard(mutex);

  ACS_TRACE("maci::LibraryManager::setupCORBAinDLL");

  if(!m_libraries.IsAllocated(i))
    return false;

  ACS_DEBUG_PARAM("maci::LibraryManager::setupCORBAinDLL", 
		  "Trying to call setup CORBA in '%s'", m_libraries[i].path.c_str());

#ifndef MAKE_VXWORKS  
  DLLSetupCORBAFunc dllSetup = (DLLSetupCORBAFunc)ACE_OS::dlsym(m_libraries[i].hLib, "DLLSetupCORBA");
#else
  DLLSetupCORBAFunc dllSetup = (DLLSetupCORBAFunc)vxworks_dlsym(m_libraries[i].hLib, "DLLSetupCORBA");
#endif

  if (dllSetup!=0)
    {
      ACS_DEBUG("maci::LibraryManager::setupCORBAinDLL", "Executing DLLSetupCORBA");
      dllSetup(m_orb, m_poaManager, m_poaRoot, m_poaPersistent, m_poaTransient);
      ACS_DEBUG("maci::LibraryManager::setupCORBAinDLL", "DLLSetupCORBA executed");
      return true;
    }

  return false;
}


bool
LibraryManager::initThreadsInDLL(int i, InitThreadFunc initFunc, DoneThreadFunc doneFunc)
{

  ACE_Guard<ACE_Recursive_Thread_Mutex> guard(mutex);

  ACS_TRACE("maci::LibraryManager::initThreadsInDLL");

  if(!m_libraries.IsAllocated(i))
    return false;

  ACS_DEBUG_PARAM("maci::LibraryManager::initThreadsInDLL",
		  "Trying to init threads in '%s'", m_libraries[i].path.c_str());

#ifndef MAKE_VXWORKS  
  DLLInitThreadsFunc dllInit = (DLLInitThreadsFunc)ACE_OS::dlsym(m_libraries[i].hLib, "DLLInitThreads");
#else
  DLLInitThreadsFunc dllInit = (DLLInitThreadsFunc)vxworks_dlsym(m_libraries[i].hLib, "DLLInitThreads");
#endif

  if (dllInit!=0)
    {
      ACS_DEBUG("maci::LibraryManager::initThreadsInDLL", "Executing DLLInitThreads");
      dllInit(initFunc, doneFunc);
      ACS_DEBUG("maci::LibraryManager::initThreadsInDLL", "DLLInitThreads executed");
      return true;
    }

  return false;
}

void LibraryManager::unload(const char* name)
{
    ACS_TRACE("maci::LibraryManager::unload");

    ACE_CString fullname = "lib";
    fullname += name;
    fullname += ACE_TEXT (ACE_DLL_SUFFIX);

    ACS_LOG(0, "maci::LibraryManager::unload", (LM_INFO, "Request to unload '%s'.", fullname.c_str()));

    int i;
    for (i = m_libraries.First(); i!=0; i = m_libraries.Next(i))
	{
	ACE_OS::printf("%s\n", m_libraries[i].path.c_str());   
	if (ACE_OS::strstr(m_libraries[i].path.c_str(), fullname.c_str())!=0)
	    {
	    unload(i);
	    return;
	    }//if
	}//for
    ACS_LOG(LM_RUNTIME_CONTEXT, "maci::LibraryManager::unload", (LM_ERROR, "Cannot find the library '%s'", fullname.c_str()));
}


void
LibraryManager::unload(int i)
{

  ACE_Guard<ACE_Recursive_Thread_Mutex> guard(mutex);

  ACS_TRACE("maci::LibraryManager::unload");

  if(m_libraries.IsAllocated(i))
    {

      ACS_DEBUG_PARAM("maci::LibraryManager::unload",
		      "Unloading '%s'", m_libraries[i].path.c_str());

#ifndef MAKE_VXWORKS
      DLLCloseFunc dllClose = (DLLCloseFunc)ACE_OS::dlsym(m_libraries[i].hLib, "DLLClose");
#else
      DLLCloseFunc dllClose = (DLLCloseFunc)vxworks_dlsym(m_libraries[i].hLib, "DLLClose");
#endif

      if (dllClose!=0)
	{
	  ACS_DEBUG("maci::LibraryManager::unload", "Executing DLLClose");
	  dllClose();
	  ACS_DEBUG("maci::LibraryManager::unload", "DLLClose executed");
	}

#ifndef MAKE_VXWORKS
      ACE_OS::dlclose(m_libraries[i].hLib);
#else
      vxworks_dlclose(m_libraries[i].hLib);
#endif

      ACS_LOG(0, "maci::LibraryManager::unload", (LM_INFO, "Unloaded '%s'.", m_libraries[i].path.c_str()));

      m_libraries.Deallocate(i);
    }

}

void *
LibraryManager::getSymbol(int i, const char * symbol)
{

  ACE_Guard<ACE_Recursive_Thread_Mutex> guard(mutex);

  ACS_TRACE("maci::LibraryManager::getSymbol");

  if(!m_libraries.IsAllocated(i))
    return 0;

#ifndef MAKE_VXWORKS
  return ACE_OS::dlsym(m_libraries[i].hLib, symbol);
#else
  return vxworks_dlsym(m_libraries[i].hLib, const_cast<char*>(symbol));
#endif

}

 }; 


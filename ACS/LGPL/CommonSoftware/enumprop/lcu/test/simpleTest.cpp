
#define ACE_MAIN boc

#include "ace/OS.h"
#include "logging.h"
#include "ace/Sched_Params.h"
#include "ace/OS_Thread_Adapter.h"


#	include "rebootLib.h"
#	include "acsUtilArgUnpack.h"

#include "symLib.h"
#include "sysSymTbl.h"
/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
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
*/

int main(int l_argc, char *l_argv[])
{
 

  LoggingProxy tlog (1, 0, 31, 0);
  LoggingProxy::init (&tlog);

  ACS_SHORT_LOG((LM_INFO,"baciTestServer: before InitCORBA"));
  CORBA::ORB_var m_orb = CORBA::ORB_init(l_argc, l_argv, "TAO");
  ACS_SHORT_LOG((LM_INFO,"baciTestServer: after InitCORBA %d", MAX_SYS_SYM_LEN));
 
  for (int i=0; i<l_argc; i++)
   ACS_SHORT_LOG((LM_INFO,"args[%d] = %s", i, l_argv[i]));

  LoggingProxy::done ();

  return 0;
}


int startTest (char *szCmdLn)
{
  int  l_argc;
  char *l_argv[100]; 

  ACE_MAIN_OBJECT_MANAGER;

  ACE_OS::printf ("Line: %s\n", szCmdLn); 
  l_argc = argUnpack(szCmdLn, l_argv);
  l_argv[0] = "simpleTest";

  return ACE_MAIN(l_argc, l_argv);
}

int
spaX (FUNCPTR entry, ...)
{
  static const unsigned int MAX_ARGS = 10;
  static char *argv[MAX_ARGS];
  va_list pvar;
  unsigned int argc;

  // Hardcode a program name because the real one isn't available
  // through the VxWorks shell.

 
  char tbuf[MAX_SYS_SYM_LEN];
  int sym;
  SYM_TYPE stype;
  

  int st = symFindByValue (sysSymTbl , (int)entry, tbuf, &sym, &stype);

  ACE_OS::printf ("Name %s \n", tbuf);
  tbuf[MAX_SYS_SYM_LEN-1] = 0;
  char *pos = strstr (tbuf, "__");
  ACE_OS::printf ("Name1 %s %d\n", tbuf, pos-tbuf);
  argv[0] = new char[pos-tbuf+1];
  strncpy (argv[0], tbuf, pos-tbuf);
  argv[0][pos-tbuf] = 0;
  ACE_OS::printf ("Name2 %s\n", argv[0]);

  // Peel off arguments to spa () and put into argv.  va_arg () isn't
  // necessarily supposed to return 0 when done, though since the
  // VxWorks shell uses a fixed number (10) of arguments, it might 0
  // the unused ones.  This function could be used to increase that
  // limit, but then it couldn't depend on the trailing 0.  So, the
  // number of arguments would have to be passed.
  va_start (pvar, entry);

  for (argc = 1; argc <= MAX_ARGS; ++argc)
    {
      argv[argc] = va_arg (pvar, char *);

      if (argv[argc] == 0)
        break;
    }

  if (argc > MAX_ARGS  &&  argv[argc-1] != 0)
    {
      // try to read another arg, and warn user if the limit was exceeded
      if (va_arg (pvar, char *) != 0)
        ACE_OS::fprintf (stderr, "spa(): number of arguments limited to %d\n",
                         MAX_ARGS);
    }
  else
    {
      // fill unused argv slots with 0 to get rid of leftovers
      // from previous invocations
      for (unsigned int i = argc; i <= MAX_ARGS; ++i)
        argv[i] = 0;
    }

  // The hard-coded options are what ::sp () uses, except for the
  // larger stack size (instead of ::sp ()'s 20000).
  const int ret = ::taskSpawn (argv[0],    // task name
                               100,        // task priority
                               VX_FP_TASK, // task options
                               ACE_NEEDS_HUGE_THREAD_STACKSIZE, // stack size
                               entry,      // entry point
                               argc,       // first argument to main ()
                               (int) argv, // second argument to main ()
                               0, 0, 0, 0, 0, 0, 0, 0);
  va_end (pvar);

  delete[] argv[0];


  // ::taskSpawn () returns the taskID on success: return 0 instead if
  // successful
  return ret > 0 ? 0 : ret;
}

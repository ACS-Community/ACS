#ifndef bacithread_h
#define bacithread_h

/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2004 
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
* "@(#) $Id: baciThread.h,v 1.102 2007/06/12 08:02:23 nbarriga Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* almamgr   2000-12-03  created
*/

/** 
 * @file 
 * Header file for BACI Thread.
 */

#include <baciExport.h>
#include <acsThreadBase.h>
#include <acsutilTimeStamp.h>

namespace baci {

/*
typdefs for backward compatiblity
 */
    typedef ACS::ThreadBase BACIThread;
    typedef ACS::ThreadManagerBase  BACIThreadManager;
    typedef ACS::ThreadBaseParameter  BACIThreadParameter;
    typedef ACS::ThreadSyncGuard ThreadSyncGuard;

#define NullBACIThread NullThreadBase
// for redirection of call getBACIThread to getThreadBase
#define getBACIThread getThreadBase
#define BACIMutex ACE_Recursive_Thread_Mutex
}; //namespace baci {

#endif






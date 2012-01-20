#ifndef logging_handler_H
#define logging_handler_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) Associated Universities Inc., 2005 
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
* "@(#) $Id: loggingHandler.h,v 1.13 2012/01/20 22:07:44 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-03-09  created
*/

/** @file loggingHandler.h
 *  Header file for abstract Handler.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <string>
#include <lokiSmartPtr.h>
#include "loggingBaseLog.h"

namespace Logging 
{
    //------------------------------------------------------------------------------
    /**
     * A Handler object takes log messages from a Logger and exports them. 
     * It might for example, write them to a console or write them to a file, 
     * or send them to a network logging service, or forward them to an OS log, 
     * or whatever.
     * A Handler can filter out certain log messages by using the setLevel
     * method.
     * This class is abstract as the developer must implement the log and getName
     * methods inherited from BaseLog.
     */
    class Handler : public virtual BaseLog
    {
      public:
	//----------------------------------------------------
	///Handler smart pointer
#ifndef MAKE_VXWORKS
	typedef Loki::SmartPtr<Handler, 
#ifdef __CYGWIN__
			       Logging::RefCounted,
#else
			       Loki::RefCountedMTAdj<Loki::ObjectLevelLockable>::RefCountedMT,
#endif
			       Loki::AllowConversion,
			       Loki::NoCheck,
			       Loki::DefaultSPStorage> HandlerSmartPtr;
#else
	typedef Loki::SmartPtr<Handler, 
			       RefCountedMT,
			       Loki::AllowConversion,
			       Loki::NoCheck,
			       Loki::DefaultSPStorage> HandlerSmartPtr;
#endif


	/**
	 * Destructor
	 */
	virtual ~Handler();
	
	/**
	 * Get the log level specifying which messages will be logged by 
	 * this Handler. Message levels lower than this level will be discarded.
	 * @return Current log level for this Handler.
	 */
	Priority 
	getLevel() const {return priority_m;}
	
	/**
	 * By utilizing setLevel, one can filter out log messages entirely
	 * based on their priority. For example, calling setLevel(DEBUG)
	 * on a Handler would imply log messages with a lower priority such
	 * as TRACE would never be passed to the publish method.
	 * @param priority New priority for this Handler
	 * @return void
	 */
	void
	setLevel(Priority priority) {
		
	priority_m = priority; }

	/**
	 * Set levels for local and remote logging. It depends on the
	 * handler implementation (does it support only loca/remote or both)
	 * to handle the level. Default implementation sets localPriority. 
	 * @see setLevel()
	 * @return void
	 */
	virtual void
	setLevels(Priority remotePriority, Priority localPriority, int type) {setLevel(remotePriority); }

    int
    getRemoteLevel() { return remotePriority_m; }
    int
    getLocalLevel() { return localPriority_m; }

    void
    setRemoteLevel(Priority remotePriority){ remotePriority_m = remotePriority;}
    void
    setLocalLevel(Priority localPriority){ localPriority_m = localPriority;}
    
    void
    setRemoteLevelType(int type){ priority_type_remote_m = type;}
    void
    setLocalLevelType(int type){ priority_type_local_m = type;}
    
    int
    getRemoteLevelType(){ return priority_type_remote_m;}
    int 
    getLocalLevelType(){ return priority_type_local_m;}
    protected:
	//----------------------------------------------------
	
      private:
	//----------------------------------------------------
	/**
	 * Current priority that needs to be met before log messages are

	 * sent to the publish method.
	 */
	Priority priority_m;
	/**
	 * Local (stdout) priority.
	 */
	Priority localPriority_m;
    int priority_type_local_m;
	int priority_type_remote_m;
	/**
	 * Remote priority.
	 */
	Priority remotePriority_m;
    };
    //------------------------------------------------------------------------------
};

#endif /*!_H*/

#ifndef _Task_H
#define _Task_H

/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2005 
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
* who       when       what
* --------  --------   ----------------------------------------------
* sharring  2005-07-14 created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#include <ParameterSet.h>
#include <TaskServices.h>

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

using Parameters::ParameterSet;

namespace ACS 
{
   /**
    * The Task class is the (abstract) base class for all tasks, whether they are ACS-based (e.g. 
	 * parameterTask) or not (e.g. future non-ACS tasks perhaps implemented by OFFLINE team).
    */
	class Task
	{
		public:
			/**
			 * go is the (required) method which has to be implemented by the implementor of the task
			 */
			virtual void go(ParameterSet& pset, const TaskServices& services)=0; //TBD: exception which can be thrown

	};
}; 
#endif /*!_H*/

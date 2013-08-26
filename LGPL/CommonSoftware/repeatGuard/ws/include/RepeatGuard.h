#ifndef REPEATGUARD_H
#define REPEATGUARD_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) Associated Universities Inc., 2007 
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
* "@(#) $Id: RepeatGuard.h,v 1.6 2008/05/21 20:25:08 nbarriga Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* nbarriga  2007-02-20  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutilTimeStamp.h>
#include "acsThread.h"

namespace repeatguard{
	const unsigned int AND = 0;
	const unsigned int OR = 1;
	const unsigned int TIMER = 2;
	const unsigned int COUNTER =3;
};

/** @class RepeatGuard 
*   @brief Guard class against code repetitions.
*
*   This class intends to be a generic class to avoid repetition of code blocks.
*   It is also intended as a base class for other specific uses, such as RepetGuardLogger. 
*
*   
*/
class RepeatGuard{
	private:
	
	protected:
		unsigned int counter;
		unsigned int counterAtLastCheck;
		unsigned int maxRepetitions;
                unsigned int method;
                bool firstTime;
		ACS::TimeInterval interval;
		ACS::Time lastTime;
                ACE_Recursive_Thread_Mutex mutex;
	public:
                /** Constructor
                * @param interval minimum interval between allowing an action(i.e. check returns true)(in 100ns)(condition 1)
                * @param maxRepetitions maxRepetitions between logs.(condition 2)
                * @param OR true: conditions 1 or 2 must be met, false: conditions 1 and 2 must be met. Ignored if interval
                *       or maxRepetitions is zero.
                */
		RepeatGuard(ACS::TimeInterval interval, unsigned int maxRepetitions, bool or_or_and=true);

		~RepeatGuard();

                /** This method returns true or false if the next block of code is allowed to be executed or not.
                *
                * @return true if the conditions to allow an action have been met(conditions depend on the constructor used)
                */
		bool check();

                /** This method returns true or false if the next block of code is allowed to be executed or not.
                *   Also it increments the counter.
                *
                * @return true if the conditions to allow an action have been met(conditions depend on the constructor used)
                */
		bool checkAndIncrement();

                /** Increments the repetition counter.
                *
                */
		void increment();

                /** To see how many attempts have been made.
                *   @return the counter at the moment of the last call to check() or checkAndIncrement()
                */
		unsigned int count();

                /** Resets counter and time of the last time check() returned true.
                */
		void reset();

                /** Resets counter and time of the last time check() returned true.
                * @param interval minimum interval between allowing an action(i.e. check returns true)(in 100ns)(condition 1)
                * @param maxRepetitions maxRepetitions between logs.(condition 2)
                * @param OR true: conditions 1 or 2 must be met, false: conditions 1 and 2 must be met. Ignored if interval
                *       or maxRepetitions is zero.
                */
		void reset(ACS::TimeInterval interval, unsigned int maxRepetitions, bool or_or_and=true);


};
#endif /*!REPEATGUARD_H*/

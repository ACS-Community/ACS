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
* "@(#) $Id: RepeatGuard.h,v 1.1 2007/02/26 13:19:41 nbarriga Exp $"
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
		ACS::Time interval;
		ACS::Time lastTime;
	public:
                /** Constructor
                * @param interval minimum interval between allowing an action(i.e. check returns true)
                * @param maxRepetitions override minimum interval if maxRepetitions is reached.(0 disables this feature)
                */
		RepeatGuard(unsigned int interval, unsigned int maxRepetitions=0);

		~RepeatGuard();

                /** This method returns true or false if the next block of code is allowed to be executed or not.
                *
                * @return true if an amount of time longer than "interval" has elapsed or
                *           maxRepetitions has been reached. Returns false in other case.
                */
		bool check();

                /** This method returns true or false if the next block of code is allowed to be executed or not.
                *   Also it increments the counter.
                *
                * @return true if an amount of time longer than "interval" has elapsed or
                *           maxRepetitions has been reached. Returns false in other case.
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
                *   @param interval minimum interval between allowing an action(i.e. check returns true)
                *   @param maxRepetitions override minimum interval if maxRepetitions is reached.(0 disables this feature)
                */
		void reset(unsigned int interval, unsigned int maxRepetitions=0);
};
#endif /*!REPEATGUARD_H*/

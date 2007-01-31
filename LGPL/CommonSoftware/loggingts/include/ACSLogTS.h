#ifndef ACSLOGTS_H
#define ACSLOGTS_H
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
* "@(#) $Id: ACSLogTS.h,v 1.1 2007/01/31 14:03:39 nbarriga Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* nbarriga  2007-01-30  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <string>
#include <acslogS.h>
#include <loggingACSLogger.h>
#include <logging.h>

/** This class is not intended to be instanciated, only subclassed
*
*/
using namespace std;

class ACSLogTS{
	private:
		Logging::BaseLog::Priority priority;
		string file;
		unsigned long line;
		string routine;
		string name;
		string shortDescription;
	protected:
		ACSLog::NVPairSeq members;

	public:
		ACSLogTS(Logging::BaseLog::Priority priority,
			string file, 
			unsigned long line, 
			string routine,
			string name,
			string shortDescription);
		~ACSLogTS();
		void log();

};

#endif /*!ACSLOGTS_H*/

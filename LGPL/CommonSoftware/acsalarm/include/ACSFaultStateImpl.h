#ifndef ACS_FAULT_STATE_IMPL_H
#define ACS_FAULT_STATE_IMPL_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2006 
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
* "@(#) $Id$"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2006-08-16  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */
 
#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <string>

#include <Properties.h>
#include <Timestamp.h>

#import "ACSFaultState.h"

using namespace std;
using namespace laserUtil;

namespace laserSource
{
	/*
	 * The Fault State for the ACS AS
	 * All the methods have a null implementation
	 */
	class ACSFaultStateImpl: public ACSFaultState 
	{	
		public:
			ACSFaultStateImpl();
			
			ACSFaultStateImpl(ACSFaultState& fs);
			
			ACSFaultStateImpl(string family, string member, int code);
			
			virtual string toXML(int amountToIndent = 3);
			virtual void setUserProperties(auto_ptr<Properties> theProperties);
	
			virtual Properties & getUserProperties();
			virtual void setUserTimestamp(auto_ptr<Timestamp> theTimestamp);
	
	
			virtual Timestamp & getUserTimestamp();
	
			virtual bool getActivatedByBackup();
	
			virtual void setActivatedByBackup(bool newActivatedByBackup);
	
			virtual bool getTerminatedByBackup();
	
			virtual void setTerminatedByBackup(bool newTerminatedByBackup);
		
		private:
			Properties props;
			Timestamp dummyTimestamp;
	};
};
#endif

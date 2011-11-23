#ifndef ASI_MESSAGE_H
#define ASI_MESSAGE_H

/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
#include <memory>
#include <string>
#include <vector>
#include "Timestamp.h"
#include "FaultState.h"

namespace acsalarm
{
	/*
	 * The class which encapsulates the data for one or more fault states (FaultState)
	 * to be sent to the laser alarm server.
	 */
	class ASIMessage
	{
		public:
			ASIMessage();
			ASIMessage(std::auto_ptr<std::vector<acsalarm::FaultState> > & faults);
			virtual ~ASIMessage();

			std::vector<acsalarm::FaultState> & getFaultStates() { return *faultStates; }
			void setFaultStates(std::auto_ptr<std::vector<acsalarm::FaultState> > & faults) { faultStates = faults; }

			bool getBackup() { return backup; }
			void setBackup(bool bkup) { backup = bkup; }

			std::string getVersion() { return version; }
			void setVersion(std::string ver) { version = ver; }

			std::string getSourceName() { return sourceName; }
			void setSourceName(std::string name) { sourceName = name; }

			std::string getSourceHostname() { return sourceHostname; }
			void setSourceHostname(std::string name) { sourceHostname = name; }

			acsalarm::Timestamp & getSourceTimestamp() { return *sourceTimestamp; }
			void setSourceTimestamp(std::auto_ptr<acsalarm::Timestamp> ts) { sourceTimestamp = ts; }

			// builds the xml representation of the message which will be sent to the alarm server
			std::string toXML();

		private:
			std::auto_ptr<std::vector<acsalarm::FaultState> > faultStates;
			bool backup;
			std::string version;
			std::string sourceName;
			std::string sourceHostname;
			std::auto_ptr<acsalarm::Timestamp> sourceTimestamp;
	};
};
#endif

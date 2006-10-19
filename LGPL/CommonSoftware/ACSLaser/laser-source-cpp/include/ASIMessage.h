#ifndef ASI_MESSAGE_H
#define ASI_MESSAGE_H

#include <memory>
#include <string>
#include <vector>
#include "Timestamp.h"
#include "FaultState.h"

using acsalarm::FaultState;
using acsalarm::Timestamp;

namespace laserSource
{
	/*
	 * The class which encapsulates the data for one or more fault states (FaultState)
	 * to be sent to the laser alarm server.
	 */
	class ASIMessage
	{
		public:
			ASIMessage();
			ASIMessage(auto_ptr<vector<FaultState> > & faults);
			virtual ~ASIMessage();

			vector<FaultState> & getFaultStates() { return *faultStates; }
			void setFaultStates(auto_ptr<vector<FaultState> > & faults) { faultStates = faults; }

			bool getBackup() { return backup; }
			void setBackup(bool bkup) { backup = bkup; }

			string getVersion() { return version; }
			void setVersion(string ver) { version = ver; }

			string getSourceName() { return sourceName; }
			void setSourceName(string name) { sourceName = name; }

			string getSourceHostname() { return sourceHostname; }
			void setSourceHostname(string name) { sourceHostname = name; }

			Timestamp & getSourceTimestamp() { return *sourceTimestamp; }
			void setSourceTimestamp(auto_ptr<Timestamp> ts) { sourceTimestamp = ts; }

			// builds the xml representation of the message which will be sent to the alarm server
			string toXML();

		private:
			auto_ptr<vector<FaultState> > faultStates;
			bool backup;
			string version;
			string sourceName;
			string sourceHostname;
			auto_ptr<Timestamp> sourceTimestamp;
	};
};
#endif

#ifndef ASI_MESSAGE_H
#define ASI_MESSAGE_H

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

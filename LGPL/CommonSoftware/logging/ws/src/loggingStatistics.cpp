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
* who       when      what
* --------  --------  ----------------------------------------------
* mmanas  2014-11-11  created
*/

#include "loggingStatistics.h"
#include <sstream>

namespace Logging {
    //-----------------------------------------------------------------------------------
    const uint32_t loggingStatistics::INITIAL_NUMBER_MESSAGES       = 0;
    const uint32_t loggingStatistics::INITIAL_NUMBER_ERRORS         = 0;
    const uint32_t loggingStatistics::DEFAULT_STATISTICS_PERIOD     = 60 * 10; // 10 minutes
    const uint32_t loggingStatistics::DEFAULT_STATISTICS_GRANULARITY= 1;       // 1 second
    const bool loggingStatistics::DEFAULT_STATISTICS_STATE              = true;    // disabled

    // Constructor
    loggingStatistics::loggingStatistics() :
    		disableStatistics(DEFAULT_STATISTICS_STATE),
    		accumulatedNumberOfMessages(INITIAL_NUMBER_MESSAGES),
    		accumulatedNumberOfLogErrors(INITIAL_NUMBER_ERRORS),
    		lastStatisticsRepportTime (getTimeStamp()),
    		statisticsCalculationPeriod(DEFAULT_STATISTICS_PERIOD),
    		statisticsGranularity (DEFAULT_STATISTICS_GRANULARITY),
    		lastPeriodNumberOfMessages(INITIAL_NUMBER_MESSAGES),
    		lastPeriodNumberOfLogErrors(INITIAL_NUMBER_ERRORS)
	{

	}

    // Statistics module methods
	void
	loggingStatistics::incrementNumberOfMessages()
	{
		// Increase value of accumulatedNumberOfMessages
		setAccumulatedNumberOfMessages(getAccumulatedNumberOfMessages() + 1);
	}

	void
	loggingStatistics::incrementNumberOfLogErrors()
	{
		// Increase value of accumaltedNumberOfLogErrors
		setAccumulatedNumberOfLogErrors(getAccumulatedNumberOfLogErrors() + 1);
	}

	void
	loggingStatistics::resetStatistics()
	{
		// Perform backup
		setLastPeriodNumberOfMessages(getAccumulatedNumberOfMessages());
		setLastPeriodNumberOfLogErrors(getAccumulatedNumberOfLogErrors());

		// Reset value of accumulatedNumberOfMessages
		setAccumulatedNumberOfMessages(INITIAL_NUMBER_MESSAGES);

		// Reset value of accumaltedNumberOfLogErrors
		setAccumulatedNumberOfLogErrors(INITIAL_NUMBER_ERRORS);

		// Set last time statistics were calculated
		setLastStatisticsRepportTime(getTimeStamp());

	}

	void
	loggingStatistics::configureStatistics(const std::string elementName,
										   const bool state,
										   const uint32_t period,
										   const uint32_t granularity)
	{
		// Construct the identification string of the statistics
		statisticsIdentification = elementName;

		// Reconfiguration means lost of current stats
		resetStatistics();

		// Configure (enable/disable) statistics module
		disableStatistics = state;

		// Configure value of statisticsCalculationPeriod
		if (period > 0)
		{
			setStatisticsCalculationPeriod(period);
		}
		else
		{
			setStatisticsCalculationPeriod(1);
		}

		// Configure value of statisticsGranularity
		if (granularity > 0)
		{
			setStatisticsGranularity(granularity);
		}
		else
		{
			setStatisticsGranularity(1);
		}
	}

    void
    loggingStatistics::calculateLoggingStatistics()
    {
    	// Calculate actual statistics period
    	actualStatisticsPeriod = (float(getTimeStamp() - getLastStatisticsRepportTime()) / float (10000000));

    	// Calculate basic statistics
		messageStatistics = (float(getAccumulatedNumberOfMessages()) * float(getStatisticsGranularity())) / actualStatisticsPeriod;
		errorStatistics = (float(getAccumulatedNumberOfLogErrors()) * float(getStatisticsGranularity())) /  actualStatisticsPeriod;

		// Calculate variations
		messageIncrement = (float(getAccumulatedNumberOfMessages()) - float(getLastPeriodNumberOfMessages()) ) *
				           (float(100) / float(getLastPeriodNumberOfMessages()) );
	    errorIncrement= (float(getAccumulatedNumberOfLogErrors()) - float(getLastPeriodNumberOfLogErrors()) ) *
		                (float(100) / float(getLastPeriodNumberOfLogErrors()) );
    }

    void
    loggingStatistics::retrieveStatisticsLogs(std::list<std::string> &statisticsLogList,
    		                                  const std::string loggerId)
	{
    	// Clear the output logs
    	statisticsLogList.clear();

    	// Temporal streamer
    	std::ostringstream oss;

		// Generate and store first log line: Separator
		oss.clear();
		oss.str(std::string());
		oss << "-------- LOGGING STATISTICS FOR: " << statisticsIdentification << "."<< loggerId;
		statisticsLogList.push_back (oss.str());

		// Generate and store log for number of messages
		oss.clear();
		oss.str(std::string());
		oss << "Total logging messages during last period: " << getLastPeriodNumberOfMessages();
		statisticsLogList.push_back (oss.str());

		// Generate and store log for messages statistics
		oss.clear();
		oss.str(std::string());
		oss << "Number of messages per " << getStatisticsGranularity() << " second(s)"
			<< " during last " << actualStatisticsPeriod << " seconds = "
			<< messageStatistics;
		statisticsLogList.push_back (oss.str());

		// Generate and store log for messages variability
		oss.clear();
		oss.str(std::string());
		oss << "Increment of messages from last period: " << messageIncrement << "%";
		statisticsLogList.push_back (oss.str());

		// Generate and store log for number of errors
		oss.clear();
		oss.str(std::string());
		oss << "Total logging errors during last period: " << getLastPeriodNumberOfLogErrors();
		statisticsLogList.push_back (oss.str());

		// Generate and store log for errors statistics
		oss.clear();
		oss.str(std::string());
		oss << "Number of errors per " << getStatisticsGranularity() << " second(s)"
			<< " during last " << actualStatisticsPeriod << " seconds = "
			<< errorStatistics;
		statisticsLogList.push_back (oss.str());

		// Generate and store log for errors variability
		oss.clear();
		oss.str(std::string());
		oss << "Increment of logging errors from last period: " << errorIncrement << "%";
		statisticsLogList.push_back (oss.str());

		// Generate and store last line log: Separator
		oss.clear();
		oss.str(std::string());
		oss << "---------------------------------------------------------------------------------";
		statisticsLogList.push_back (oss.str());
	}

    // Getters and setters implementation
    bool
    loggingStatistics::getDisableStatistics()
    {
    	return disableStatistics;
    };

    // Getter for statisticsIdentification
    std::string
	loggingStatistics::getStatisticsIdentification()
    {
    	return statisticsIdentification;
    };

    uint32_t
    loggingStatistics::getAccumulatedNumberOfMessages()
	{
		return accumulatedNumberOfMessages;
	};

    uint32_t
    loggingStatistics::getAccumulatedNumberOfLogErrors()
	{
		return accumulatedNumberOfLogErrors;
	};

    uint64_t
	loggingStatistics::getLastStatisticsRepportTime()
	{
		return lastStatisticsRepportTime;
	};

	uint32_t
	loggingStatistics::getStatisticsCalculationPeriod()
	{
		return statisticsCalculationPeriod;
	};

	uint32_t
	loggingStatistics::getStatisticsGranularity()
	{
		return statisticsGranularity;
	};

	uint32_t
	loggingStatistics::getLastPeriodNumberOfMessages()
	{
		return lastPeriodNumberOfMessages;
	};

	uint32_t
	loggingStatistics::getLastPeriodNumberOfLogErrors()
	{
		return lastPeriodNumberOfLogErrors;
	};

	void
	loggingStatistics::setAccumulatedNumberOfMessages(uint32_t value)
	{
		accumulatedNumberOfMessages = value;
	};

	void
	loggingStatistics::setAccumulatedNumberOfLogErrors(uint32_t value)
	{
		accumulatedNumberOfLogErrors = value;
	};

	void
	loggingStatistics::setLastStatisticsRepportTime(uint64_t value)
	{
		lastStatisticsRepportTime = value;
	};

	void
	loggingStatistics::setStatisticsCalculationPeriod(uint32_t value)
	{
		statisticsCalculationPeriod = value;
	};

	void
	loggingStatistics::setStatisticsGranularity(uint32_t value)
	{
		statisticsGranularity = value;
	};

	void
	loggingStatistics::setLastPeriodNumberOfMessages(uint32_t value)
	{
		lastPeriodNumberOfMessages = value;
	};

	void
	loggingStatistics::setLastPeriodNumberOfLogErrors(uint32_t value)
	{
		lastPeriodNumberOfLogErrors = value;
	};
};

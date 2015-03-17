#ifndef logging_statistics_H
#define logging_statistics_H
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

/** @file loggingStatistics.h
 *  Header file for statistics class used by C++ ACS logging.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acsutilTimeStamp.h"
#include <list>
#include <string>
#include <stdint.h>

namespace Logging 
{
	// This class includes all cpp statistics aspects of the ACS logging system
    class loggingStatistics
    {
    private:
    	// Boolean defining if the statistics module is active or inactive
    	bool disableStatistics;
    	// Identification string of the statistics. Composed by component name + logger name
    	std::string statisticsIdentification;

    	// Number of valid messages logged during last period
    	uint32_t accumulatedNumberOfMessages;
    	// Number of logging errors generated during last period
    	uint32_t accumulatedNumberOfLogErrors;
    	// Last time instant of statistics calculation and repporting [timestamp]
    	uint64_t lastStatisticsRepportTime;
    	// Time between statistics calculation and repporting [in seconds] Default = 10 minutes
    	uint32_t statisticsCalculationPeriod;
    	// Granularity of the statistics [in seconds]. Default = 1 second
    	uint32_t statisticsGranularity;
    	// Last period backup of accumulatedNumberOfMessages
    	uint32_t lastPeriodNumberOfMessages;
    	// Last period backup of accumaltedNumberOfLogErrors
    	uint32_t lastPeriodNumberOfLogErrors;

    	// Statistics calculation
    	float messageStatistics; 		// Calculated message statistics
		float errorStatistics;			// Calculated error statistics
		float messageIncrement;			// Calculated increment of messages
		float errorIncrement;			// Calculated increment of logging errors
		float actualStatisticsPeriod;	// Actual statistics calculation period

    public:
		// Getter for disableStatistics
		bool getDisableStatistics();

		// Getter for statisticsIdentification
		std::string getStatisticsIdentification();

    	// Getter for accumulatedNumberOfMessages
		uint32_t getAccumulatedNumberOfMessages();

    	// Getter for accumaltedNumberOfLogErrors
		uint32_t getAccumulatedNumberOfLogErrors();

    	// Getter for statisticsGranularity
		uint32_t getStatisticsGranularity();

    	// Getter for lastPeriodNumberOfMessages
		uint32_t getLastPeriodNumberOfMessages();

    	// Getter for lastPeriodNumberOfLogErrors
		uint32_t getLastPeriodNumberOfLogErrors();

    	// Getter for lastStatisticsRepportTime
		uint64_t getLastStatisticsRepportTime();

    	// Getter for statisticsCalculationPeriod
		uint32_t getStatisticsCalculationPeriod();

    	// Setter for accumulatedNumberOfMessages
		void setAccumulatedNumberOfMessages(uint32_t value);

		// Setter for accumulatedNumberOfLogErrors
		void setAccumulatedNumberOfLogErrors(uint32_t value);

		// Setter for lastStatisticsRepportTime
		void setLastStatisticsRepportTime(uint64_t value);

    	// Setter for statisticsCalculationPeriod
		void setStatisticsCalculationPeriod(uint32_t value);

    	// Setter for statisticsGranularity
		void setStatisticsGranularity(uint32_t value);

    	// Setter for lastPeriodNumberOfMessages
		void setLastPeriodNumberOfMessages(uint32_t value);

    	// Setter for lastPeriodNumberOfMessages
		void setLastPeriodNumberOfLogErrors(uint32_t value);

        // Default constructor
		loggingStatistics();

    	// Default destructor
    	virtual ~loggingStatistics(){}

		/**
		 * This method calculates the logging statistics
		 * @return void
		 */
	    virtual void
		calculateLoggingStatistics();

		/**
		 * This method retrieves the logging statistics
		 * @param statisticsLogList List of logs (strings)
		 * @param loggerId String to indintify the logger (normaly will consist on logger name + container name
		 * @return void
		 */
		virtual void
		retrieveStatisticsLogs(std::list<std::string> &statisticsLogList,
                               const std::string loggerId);

		/**
		 * This method increments the stored number of correctly transferred logs
		 * @return void
		 */
		virtual void incrementNumberOfMessages();

		/**
		 * This method increments the stored number of logging errors detected
		 * @return void
		 */
		virtual void incrementNumberOfLogErrors();

		/**
		 * This method makes a backup and resets the current statisctics values
		 * @return void
		 */
		virtual void resetStatistics();

		/**
		 * This method allows the statistic module to be configured
		 * @param elementName String defining the name of the module the statistics belong to
		 * @param state Configuaration of disableStatistics attribute (to enable / diable statistics module)
		 * @param period Configuration of statisticsCalculationPeriod
		 * @param granularity Configuration of statisticsGranularity
		 * @return void
		 */

		virtual void configureStatistics(const std::string elementName,
				                         const bool state,
				                         const uint32_t period,
				                         const uint32_t granularity);

		/**
		 * These constant members represent the default values of the statistics parametres.
		 */
		static const uint32_t INITIAL_NUMBER_MESSAGES;
		static const uint32_t INITIAL_NUMBER_ERRORS;
		static const uint32_t DEFAULT_STATISTICS_PERIOD;
		static const uint32_t DEFAULT_STATISTICS_GRANULARITY;
		static const bool DEFAULT_STATISTICS_STATE;

		};

};

#endif /*logging_statistics_H*/

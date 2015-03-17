/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.logging.statistics;

import java.util.List;

/**
 * This class takes responsibility of a logger statistics calculation and generation of
 * statistics messages.
 * 
 * Design notes: The statistics class is by default disabled, and needs to be activated 
 * if in need of statistics calculation (by means of the configuration() method. 
 * Otherwise, if statistics are inactive, the logger performances are not affected.
 * 
 * @author mmanas
 * created Nov 17, 2014 3:00:00 PM
 */
public class AcsLoggerStatistics {

	// Boolean defining if the statistics module is active or inactive
	private Boolean disableStatistics;

	// Identification string of the statistics. Composed by component name + logger name
	private String statisticsIdentification;

	// Number of valid messages logged during last period
	private int accumulatedNumberOfMessages;
	
	// Number of logging errors generated during last period
	private int accumulatedNumberOfLogErrors;

	// Last time instant of statistics calculation and repporting [timestamp]
	private long lastStatisticsRepportTime;
	
	// Time between statistics calculation and repporting [in seconds] Default = 10 minutes
	private int statisticsCalculationPeriod;
	
	// Granularity of the statistics [in seconds]. Default = 1 second
	private int statisticsGranularity;
	
	// Last period backup of accumulatedNumberOfMessages
	private int lastPeriodNumberOfMessages;
	
	// Last period backup of accumaltedNumberOfLogErrors
	private int lastPeriodNumberOfLogErrors;
	
	// Statistics calculation
	private float messageStatistics; 		// Calculated message statistics
	private float errorStatistics;			// Calculated error statistics
	private float messageIncrement;			// Calculated increment of messages
	private float errorIncrement;			// Calculated increment of logging errors
	private float actualStatisticsPeriod;	// Actual statistics calculation period
	       
	/**
	 * Standard constructor
	 */
	public AcsLoggerStatistics() {
		disableStatistics = DEFAULT_STATISTICS_STATE;
		accumulatedNumberOfMessages = INITIAL_NUMBER_MESSAGES;
		accumulatedNumberOfLogErrors = INITIAL_NUMBER_ERRORS;
		lastStatisticsRepportTime = System.currentTimeMillis();
		statisticsCalculationPeriod = DEFAULT_STATISTICS_PERIOD;
		statisticsGranularity = DEFAULT_STATISTICS_GRANULARITY;
		lastPeriodNumberOfMessages = INITIAL_NUMBER_MESSAGES;
		lastPeriodNumberOfLogErrors = INITIAL_NUMBER_ERRORS;
	}

	/**
	 * This method calculates the logging statistics
	 * @return void
	 */
    public void calculateLoggingStatistics() {
    	// Calculate actual statistics period
    	actualStatisticsPeriod = (float)(System.currentTimeMillis() - getLastStatisticsRepportTime()) / (float)(1000);

    	// Calculate basic statistics
		messageStatistics = (float)(getAccumulatedNumberOfMessages() * getStatisticsGranularity()) / actualStatisticsPeriod;
		errorStatistics = (float)(getAccumulatedNumberOfLogErrors() * getStatisticsGranularity()) /  actualStatisticsPeriod;

		// Calculate variations
		messageIncrement = (float)(getAccumulatedNumberOfMessages() - getLastPeriodNumberOfMessages() ) *
				           ((float)(100) / (float)(getLastPeriodNumberOfMessages()) );
	    errorIncrement= (float)(getAccumulatedNumberOfLogErrors() - getLastPeriodNumberOfLogErrors() ) *
		                ((float)(100) / (float)(getLastPeriodNumberOfLogErrors()) );
    }
	
	/**
	 * This method generates the logging statistics in message format
	 * @param statisticsLogList
	 *            List of logs (strings)
	 * @param loggerId
	 *            String to indintify the logger
	 *            (normaly will consist on container name + logger name)
	 * @return void
	 */
	public void retrieveStatisticsLogs(List<String> statisticsLogList,
                                       String loggerId) {
    	// Clear the output logs
    	statisticsLogList.clear();

		// Generate and store first log line: Separator
		statisticsLogList.add ("-------- LOGGING STATISTICS FOR: " + statisticsIdentification + "." + loggerId);

		// Generate and store log for number of messages
		statisticsLogList.add ("Total logging messages during last period: " + getLastPeriodNumberOfMessages());

		// Generate and store log for messages statistics
		statisticsLogList.add ("Number of messages per " + getStatisticsGranularity()
				+ " second(s)" + " during last " + actualStatisticsPeriod + " seconds = "
				+ messageStatistics);

		// Generate and store log for messages variability
		statisticsLogList.add ("Increment of messages from last period: " + messageIncrement + "%");

		// Generate and store log for number of errors
		statisticsLogList.add ("Total logging errors during last period: " + getLastPeriodNumberOfLogErrors());

		// Generate and store log for errors statistics
		statisticsLogList.add ("Number of errors per " + getStatisticsGranularity() + " second(s)"
				+ " during last " + actualStatisticsPeriod + " seconds = " + errorStatistics);

		// Generate and store log for errors variability
		statisticsLogList.add ("Increment of logging errors from last period: " + errorIncrement + "%");

		// Generate and store last line log: Separator
		statisticsLogList.add ("---------------------------------------------------------------------------------");
	}

	/**
	 * This method increments the stored number of correctly transferred logs
	 * @return void
	 */
	public void incrementNumberOfMessages() {
		// Increase value of accumulatedNumberOfMessages
		setAccumulatedNumberOfMessages(getAccumulatedNumberOfMessages() + 1);
	}

	/**
	 * This method increments the stored number of logging errors detected
	 * @return void
	 */
	public void incrementNumberOfLogErrors() {
		// Increase value of accumaltedNumberOfLogErrors
		setAccumulatedNumberOfLogErrors(getAccumulatedNumberOfLogErrors() + 1);
	}

	/**
	 * This method makes a backup and resets the current statisctics values
	 * @return void
	 */
	public void resetStatistics() {
		// Perform backup
		setLastPeriodNumberOfMessages(getAccumulatedNumberOfMessages());
		setLastPeriodNumberOfLogErrors(getAccumulatedNumberOfLogErrors());

		// Reset value of accumulatedNumberOfMessages
		setAccumulatedNumberOfMessages(INITIAL_NUMBER_MESSAGES);

		// Reset value of accumaltedNumberOfLogErrors
		setAccumulatedNumberOfLogErrors(INITIAL_NUMBER_ERRORS);

		// Set last time statistics were calculated
		setLastStatisticsRepportTime(System.currentTimeMillis());
	}

	/**
	 * This method allows the statistic module to be configured
	 * @param elementName
	 *            String defining the name of the module the statistics belong to
	 * @param state
	 *            Configuaration of disableStatistics attribute (to enable / diable statistics module)
	 * @param period
	 *            Configuration of statisticsCalculationPeriod
	 * @param granularity
	 *            Configuration of statisticsGranularity
	 * @return void
	 */

	public void configureStatistics(String elementName,
			                        boolean state,
			                        int period,
			                        int granularity) {
		// Construct the identification string of the statistics
		setStatisticsIdentification(elementName);

		// Reconfiguration means lost of current stats
		resetStatistics();

		// Configure (enable/disable) statistics module
		setDisableStatistics(state);

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

	/**
	 * These constant members represent the default values of the statistics parametres.
	 */
	public static final int INITIAL_NUMBER_MESSAGES = 0;
	public static final int INITIAL_NUMBER_ERRORS = 0;
	public static final int DEFAULT_STATISTICS_PERIOD = 10 * 60; 	// 10 minutes
	public static final int DEFAULT_STATISTICS_GRANULARITY = 1;  	// 1 second
	public static final boolean DEFAULT_STATISTICS_STATE = true;    // Disabled

	public Boolean getDisableStatistics() {
		return disableStatistics;
	}

	public String getStatisticsIdentification() {
		return statisticsIdentification;
	}

	public int getAccumulatedNumberOfMessages() {
		return accumulatedNumberOfMessages;
	}

	public int getAccumulatedNumberOfLogErrors() {
		return accumulatedNumberOfLogErrors;
	}

	public long getLastStatisticsRepportTime() {
		return lastStatisticsRepportTime;
	}

	public int getStatisticsCalculationPeriod() {
		return statisticsCalculationPeriod;
	}

	public int getStatisticsGranularity() {
		return statisticsGranularity;
	}

	public int getLastPeriodNumberOfMessages() {
		return lastPeriodNumberOfMessages;
	}

	public int getLastPeriodNumberOfLogErrors() {
		return lastPeriodNumberOfLogErrors;
	}

	public float getMessageStatistics() {
		return messageStatistics;
	}

	public float getErrorStatistics() {
		return errorStatistics;
	}

	public float getMessageIncrement() {
		return messageIncrement;
	}

	public float getErrorIncrement() {
		return errorIncrement;
	}

	public float getActualStatisticsPeriod() {
		return actualStatisticsPeriod;
	}

	public void setDisableStatistics(Boolean disableStatistics) {
		this.disableStatistics = disableStatistics;
	}

	public void setStatisticsIdentification(String statisticsIdentification) {
		this.statisticsIdentification = statisticsIdentification;
	}

	public void setAccumulatedNumberOfMessages(int accumulatedNumberOfMessages) {
		this.accumulatedNumberOfMessages = accumulatedNumberOfMessages;
	}

	public void setAccumulatedNumberOfLogErrors(int accumulatedNumberOfLogErrors) {
		this.accumulatedNumberOfLogErrors = accumulatedNumberOfLogErrors;
	}

	public void setLastStatisticsRepportTime(long lastStatisticsRepportTime) {
		this.lastStatisticsRepportTime = lastStatisticsRepportTime;
	}

	public void setStatisticsCalculationPeriod(int statisticsCalculationPeriod) {
		this.statisticsCalculationPeriod = statisticsCalculationPeriod;
	}

	public void setStatisticsGranularity(int statisticsGranularity) {
		this.statisticsGranularity = statisticsGranularity;
	}

	public void setLastPeriodNumberOfMessages(int lastPeriodNumberOfMessages) {
		this.lastPeriodNumberOfMessages = lastPeriodNumberOfMessages;
	}

	public void setLastPeriodNumberOfLogErrors(int lastPeriodNumberOfLogErrors) {
		this.lastPeriodNumberOfLogErrors = lastPeriodNumberOfLogErrors;
	}

	public void setMessageStatistics(float messageStatistics) {
		this.messageStatistics = messageStatistics;
	}

	public void setErrorStatistics(float errorStatistics) {
		this.errorStatistics = errorStatistics;
	}

	public void setMessageIncrement(float messageIncrement) {
		this.messageIncrement = messageIncrement;
	}

	public void setErrorIncrement(float errorIncrement) {
		this.errorIncrement = errorIncrement;
	}

	public void setActualStatisticsPeriod(float actualStatisticsPeriod) {
		this.actualStatisticsPeriod = actualStatisticsPeriod;
	}
       
}

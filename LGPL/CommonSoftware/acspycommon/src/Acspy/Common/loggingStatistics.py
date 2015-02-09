#    ALMA - Atacama Large Millimiter Array
#    (c) Associated Universities, Inc. Washington DC, USA,  2001
#    (c) European Southern Observatory, 2002
#    Copyright by ESO (in the framework of the ALMA collaboration)
#    and Cosylab 2002, All rights reserved
#
#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU Lesser General Public
#    License as published by the Free Software Foundation; either
#    version 2.1 of the License, or (at your option) any later version.
#
#    This library is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    Lesser General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public
#    License along with this library; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
#    who       when      what
#    --------  --------  ----------------------------------------------
#    mmanas  2014-12-01  created

'''
An interface to logging statistics calculation, including the statistics messages generation.

This module is designed to be used by the Python logger code, and to be
failsafe, so that the loggers task is guaranteed in (nearly)
all circumstances. Design choices derived from the design goals:
1) by default the logger statistics are disabled. Enabling them is an active
decision,
2) in case of being the logger statistics disable no impact over logger performances
is to be noted
3) no environment variables are configured / allowed for this module
4) each logger cames by default with its statistics module
'''

import time

class LoggingStatistics(object):
    '''
    Logging statistics is used to keep track of the statistics of a logger class.
    By default a the statistics calculation is disabled, so there is no impact over
    system performances.
    '''
    # Constants defined for the class
    CONST_DEFAULT_STATISTICS_STATE = True
    CONST_INITIAL_NUMBER_MESSAGES = 0
    CONST_INITIAL_NUMBER_ERRORS = 0
    CONST_DEFAULT_STATISTICS_PERIOD = 10 * 60 # 10 minutes 
    CONST_DEFAULT_STATISTICS_GRANULARITY = 1  # 1 second
    
    #------------------------------------------------------------------------
    def __init__(self):
        '''
        Create a Logger statistics instance.

        Parameters: Nothing

        Returns: Nothing

        Raises: Nothing
        '''
        # initialization of class attributes
        self._disableStatistics = self.CONST_DEFAULT_STATISTICS_STATE
        self._accumulatedNumberOfMessages = self.CONST_INITIAL_NUMBER_MESSAGES
        self._accumulatedNumberOfLogErrors = self.CONST_INITIAL_NUMBER_ERRORS
        self._lastStatisticsRepportTime = time.time()
        self._statisticsCalculationPeriod = self.CONST_DEFAULT_STATISTICS_PERIOD
        self._statisticsGranularity = self.CONST_DEFAULT_STATISTICS_GRANULARITY
        self._lastPeriodNumberOfMessages = self.CONST_INITIAL_NUMBER_MESSAGES
        self._lastPeriodNumberOfLogErrors = self.CONST_INITIAL_NUMBER_ERRORS
        
        # Identification string of the statistics. Composed by component name + logger name
        self._statisticsIdentification = ''
        
        # Statistics calculation initialisations
        self.messageStatistics = 0.0
        self.errorStatistics = 0.0
        self.messageIncrement = 0.0
        self.errorIncrement = 0.0
        self.actualStatisticsPeriod = 0.0
    #------------------------------------------------------------------------ 

    # Getter for statisticsIdentification
    def getStatisticsIdentification(self):
        return self._statisticsIdentification

    # Getter for disableStatistics  
    def getDisableStatistics(self):
        return self._disableStatistics

    # Getter for accumulatedNumberOfMessages        
    def getAccumulatedNumberOfMessages(self):
        return self._accumulatedNumberOfMessages

    # Setter for accumulatedNumberOfMessages
    def setAccumulatedNumberOfMessages(self, value):
        self._accumulatedNumberOfMessages = value 

    # Getter for accumulatedNumberOfLogErrors        
    def getAccumulatedNumberOfLogErrors(self):
        return self._accumulatedNumberOfLogErrors

    # Setter for accumulatedNumberOfLogErrors
    def setAccumulatedNumberOfLogErrors(self, value):
        self._accumulatedNumberOfLogErrors = value 
        
    # Getter for lastStatisticsRepportTime
    def getLastStatisticsRepportTime(self):
        return self._lastStatisticsRepportTime

    # Setter for lastStatisticsRepportTime
    def setLastStatisticsRepportTime(self, value):
        self._lastStatisticsRepportTime = value

    # Getter for statisticsCalculationPeriod
    def getStatisticsCalculationPeriod(self):
        return self._statisticsCalculationPeriod

    # Setter for statisticsCalculationPeriod
    def setStatisticsCalculationPeriod(self, value):
        self._statisticsCalculationPeriod = value 

    # Getter for statisticsGranularity
    def getStatisticsGranularity(self):
        return self._statisticsGranularity

    # Setter for statisticsGranularity
    def setStatisticsGranularity(self, value):
        self._statisticsGranularity = value 

    # Getter for lastPeriodNumberOfMessage
    def getLastPeriodNumberOfMessages(self):
        return self._lastPeriodNumberOfMessages

    # Setter for lastPeriodNumberOfMessage
    def setLastPeriodNumberOfMessages(self, value):
        self._lastPeriodNumberOfMessages = value 

    # Getter for lastPeriodNumberOfLogErrors
    def getLastPeriodNumberOfLogErrors(self):
        return self._lastPeriodNumberOfLogErrors

    # Setter for lastPeriodNumberOfLogErrors
    def setLastPeriodNumberOfLogErrors(self, value):
        self._lastPeriodNumberOfLogErrors = value 

    #------------------------------------------------------------------------

    def calculateLoggingStatistics(self):
        '''
        This method calculates the logging statistics.
        
        Parameters: Nothing
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        
        # Calculate actual statistics period
        self.actualStatisticsPeriod = (time.time() - self.getLastStatisticsRepportTime())

        # Calculate basic statistics
        self.messageStatistics = (self.getAccumulatedNumberOfMessages() * self.getStatisticsGranularity()) / self.actualStatisticsPeriod
        self.errorStatistics = (self.getAccumulatedNumberOfLogErrors() * self.getStatisticsGranularity()) /  self.actualStatisticsPeriod

        # Calculate variations
        if self.getLastPeriodNumberOfMessages() != 0:
            self.messageIncrement = (self.getAccumulatedNumberOfMessages() - self.getLastPeriodNumberOfMessages() ) * (100 / self.getLastPeriodNumberOfMessages() )
        else:
            self.messageIncrement = 0
        if self.getLastPeriodNumberOfLogErrors() != 0:
            self.errorIncrement= (self.getAccumulatedNumberOfLogErrors() - self.getLastPeriodNumberOfLogErrors() ) * (100 / self.getLastPeriodNumberOfLogErrors() )
        else:
            self.errorIncrement = 0
                
    #------------------------------------------------------------------------

    def incrementNumberOfMessages(self):
        '''
        This method increases the value of the attribute that counts the number of messages
        for the last period.
            
        Parameters: Nothing
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        # Increase value of accumulatedNumberOfMessages
        self.setAccumulatedNumberOfMessages(self.getAccumulatedNumberOfMessages() + 1)

    def incrementNumberOfLogErrors(self):
        '''
        This method increases the value of the attribute that counts the number of logging errors
        for the last period.
            
        Parameters: Nothing
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        # Increase value of accumulatedNumberOfMessages
        self.setAccumulatedNumberOfLogErrors(self.getAccumulatedNumberOfLogErrors() + 1)
    
    def resetStatistics(self):
        '''
        This method makes a backup and resets the current statistics values.
            
        Parameters: Nothing
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        # Perform backup
        self.setLastPeriodNumberOfMessages(self.getAccumulatedNumberOfMessages());
        self.setLastPeriodNumberOfLogErrors(self.getAccumulatedNumberOfLogErrors());
        
        # Reset value of accumulatedNumberOfMessages
        self.setAccumulatedNumberOfMessages(self.CONST_INITIAL_NUMBER_MESSAGES);
        
        # Reset value of accumaltedNumberOfLogErrors
        self.setAccumulatedNumberOfLogErrors(self.CONST_INITIAL_NUMBER_ERRORS);
        
        # Set last time statistics were calculated
        self.setLastStatisticsRepportTime(time.time());

    def configureStatistics(self, elementName, state, period, granularity):
        '''
        This method allows the statistic module to be configured.
            
        Parameters:
            - elementName String defining the name of the module the statistics belong to
            - state Configuration of disableStatistics attribute (to enable / diable statistics module)
            - period Configuration of statisticsCalculationPeriod
            - granularity Configuration of statisticsGranularity
        
        Returns: Nothing
        
        Raises: Nothing
        '''
        # Construct the identification string of the statistics
        self._statisticsIdentification = elementName
        
        # Reconfiguration means lost of current stats
        self.resetStatistics();
        
        # Configure (enable/disable) statistics module
        self._disableStatistics = state
        
        # Configure value of statisticsCalculationPeriod
        if period > 0:
            self.setStatisticsCalculationPeriod(period)
        else:
            self.setStatisticsCalculationPeriod(1)

        # Configure value of statisticsGranularity
        if granularity > 0:
            self.setStatisticsGranularity(granularity)
        else:
            self.setStatisticsGranularity(1)

    def retrieveStatisticsLogs(self, loggerId):
        '''
        This method retrieves the logging statistics.
            
        Parameters:
            - loggerId String to indentify the logger (normaly will consist on logger name + container name
        
        Returns:
            - statisticsLogList List of logs (strings)
        
        Raises: Nothing
        '''
        # Clear the output logs
        statisticsLogList = []

        # Generate and store first log line: Separator
        logMsg = "-------- LOGGING STATISTICS FOR: " + self.getStatisticsIdentification() + "." + loggerId
        statisticsLogList.append (logMsg)
       
        # Generate and store log for number of messages
        logMsg = "Total logging messages during last period: " + repr(self.getLastPeriodNumberOfMessages())
        statisticsLogList.append (logMsg)

        # Generate and store log for messages statistics
        logMsg = "Number of messages per " + repr(self.getStatisticsGranularity()) + " second(s)" + \
            " during last " + repr(self.actualStatisticsPeriod) + " seconds = " + \
            repr(self.messageStatistics)
        statisticsLogList.append (logMsg)

        # Generate and store log for messages variability
        logMsg = "Increment of messages from last period: " + repr(self.messageIncrement) + "%"
        statisticsLogList.append (logMsg)

        # Generate and store log for number of errors
        logMsg = "Total logging errors during last period: " + repr(self.getLastPeriodNumberOfLogErrors())
        statisticsLogList.append (logMsg)

        # Generate and store log for errors statistics
        logMsg = "Number of errors per " + repr(self.getStatisticsGranularity()) + " second(s)" \
            + " during last " + repr(self.actualStatisticsPeriod) + " seconds = " \
            + repr(self.errorStatistics)
        statisticsLogList.append (logMsg)

        # Generate and store log for errors variability
        logMsg = "Increment of logging errors from last period: " + repr(self.errorIncrement) + "%"
        statisticsLogList.append (logMsg)

        # Generate and store last line log: Separator
        logMsg = "---------------------------------------------------------------------------------"
        statisticsLogList.append (logMsg)

        return statisticsLogList
        



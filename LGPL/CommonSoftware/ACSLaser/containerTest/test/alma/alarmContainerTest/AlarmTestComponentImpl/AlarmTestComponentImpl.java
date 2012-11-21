/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2012
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

package alma.alarmContainerTest.AlarmTestComponentImpl;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.component.ComponentImplBase;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.alarmContainerTest.AlarmTestComponentOperations;

/**
 * Implementation of the locally defined AlarmTestComponent interface.
 */
public class AlarmTestComponentImpl extends ComponentImplBase implements AlarmTestComponentOperations
{

	@Override
	public void logBurst(short logLevel, int numLogs) throws CouldntPerformActionEx {
		
		try {
			AcsLogLevelDefinition coreLevel = AcsLogLevelDefinition.fromInteger(logLevel);
			AcsLogLevel level = AcsLogLevel.fromAcsCoreLevel(coreLevel);
			
			String msg = "Got call to logBurst, with log level=" + coreLevel.name + ", numLogs=" + numLogs;
			msg += ", property alma.acs.logging.lossless=" + System.getProperty("alma.acs.logging.lossless");
			msg += ", maxLogQueueSize=" + ClientLogManager.getAcsLogManager().getLogConfig().getMaxLogQueueSize();
			msg += ", maxLogsPerSecond=" + ClientLogManager.getAcsLogManager().getLogConfig().getMaxLogsPerSecond() + ".";
			m_logger.info(msg);
			
			// sleep a bit, so that the above log gets processed before the alarm throttle can remove it.
			Thread.sleep(100);
			
			for (int i = 0; i < numLogs; i++) {
				m_logger.log(level, "Test log (" + coreLevel.toString() + ") #" + i);
			}
		} catch (Exception ex) {
			throw (new AcsJCouldntPerformActionEx(ex)).toCouldntPerformActionEx();
		} finally {
			// sleep for one log throttle interval (LogThrottle.LogStreamThrottle#intervalLengthMillis), to ensure  
			// that the subsequent component release logs make the log throttle re-evaluate the load situation,
			// which will then lead to the throttle alarm getting cleared.
			// Otherwise the alarm would only get cleared when the container outputs some status log or shuts down.
			// This is a special issue of this test (in the real world there are always some logs...) and does not 
			// seem to justify adding an "evaluation thread" to the LogThrottle.
			try {
				Thread.sleep(1000);
			} catch (InterruptedException ex) {
				ex.printStackTrace();
			}
		}
		
	}

}

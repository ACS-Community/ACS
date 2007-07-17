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

package alma.acs.logging.domainspecific;

import java.util.logging.Level;

import alma.acs.logging.AcsLogRecord;
import alma.acs.logging.AcsLogger;

public class AntennaContextLogger {

	private final AcsLogger delegateLogger;

	public AntennaContextLogger(AcsLogger logger) {
		delegateLogger = logger;
		delegateLogger.addLoggerClass(getClass());
	}

	/**
	 * @param level
	 * @param msg
	 * @param audience  Should be one of the IDL-defined string constants from package <code>alma.log_audience</code>,
	 *                  or any string in case a required audience type has not yet been defined in IDL.
	 *                  May also be <code>null</code>, but {@link alma.log_audience.NONE.value} is preferred.  
	 * @param array
	 * @param antenna
	 */
	public void log(Level level, String msg, String audience, String array, String antenna) {
		AcsLogRecord lr = delegateLogger.createAcsLogRecord(level, msg);
		lr.setAudience(audience);
		lr.setArray(array);
		lr.setAntenna(antenna);
		delegateLogger.log(lr);
	}

	/**
	 * @param level
	 * @param msg
	 * @param thr
	 * @param audience  Should be one of the IDL-defined string constants from package <code>alma.log_audience</code>,
	 *                  or any string in case a required audience type has not yet been defined in IDL.
	 *                  May also be <code>null</code>, but {@link alma.log_audience.NONE.value} is preferred.  
	 * @param array
	 * @param antenna
	 */
	public void log(Level level, String msg, Throwable thr, String audience, String array, String antenna) {
		AcsLogRecord lr = delegateLogger.createAcsLogRecord(level, msg);
		lr.setThrown(thr);
		lr.setAudience(audience);
		lr.setArray(array);
		lr.setAntenna(antenna);
		delegateLogger.log(lr);
	}

	/**
	 * @param level
	 * @param msg
	 * @param array
	 * @param antenna
	 */
	public void log(Level level, String msg, String array, String antenna) {
		AcsLogRecord lr = delegateLogger.createAcsLogRecord(level, msg);
		lr.setArray(array);
		lr.setAntenna(antenna);
		delegateLogger.log(lr);
	}

	/**
	 * @param level
	 * @param msg
	 * @param thr
	 * @param array
	 * @param antenna
	 */
	public void log(Level level, String msg, Throwable thr, String array, String antenna) {
		AcsLogRecord lr = delegateLogger.createAcsLogRecord(level, msg);
		lr.setThrown(thr);
		lr.setArray(array);
		lr.setAntenna(antenna);
		delegateLogger.log(lr);
	}
}

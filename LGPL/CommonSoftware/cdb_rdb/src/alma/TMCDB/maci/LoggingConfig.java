/*
 * ALMA - Atacama Large Millimeter Array
 * (c) European Southern Observatory, 2002
 * (c) Associated Universities Inc., 2002
 * Copyright by ESO (in the framework of the ALMA collaboration),
 * Copyright by AUI (in the framework of the ALMA collaboration),
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY, without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307  USA
 * File LoggingConfig.java
 */
package alma.TMCDB.maci;

import java.util.Map;

@SuppressWarnings("serial")
public class LoggingConfig implements java.io.Serializable {
    static private final String newline = System.getProperty("line.separator");

    @SuppressWarnings("unused")
	private int LoggingConfigId;
    
    private Integer minLogLevel;
    private Integer minLogLevelLocal;
    private String centralizedLogger;
    private Integer dispatchPacketSize;
    private Integer immediateDispatchLevel;
    private Integer flushPeriodSeconds;
    private Integer maxLogQueueSize;
    private Integer maxLogsPerSecond;
    
	// must be public to be accessible, but should not have getter to be come visible as node
	public Map<String, NamedLoggerConfig> _;
    
    /**
     * Default Constructor for LoggingConfig.  Setter methods must be used to insert data.
     */
    public LoggingConfig () {
    }

	@Override
	public String toString() {
		return "LoggingConfig:" + newline 
				+ "LoggingConfigId: " + LoggingConfigId + newline
				+ "minLogLevel: " + minLogLevel 
				+ "minLogLevelLocal: " + minLogLevelLocal  + newline 
				+ "centralizedLogger: " + centralizedLogger + newline
				+ "dispatchPacketSize: " + dispatchPacketSize + newline
				+ "immediateDispatchLevel: " + immediateDispatchLevel + newline
				+ "flushPeriodSeconds: " + flushPeriodSeconds + newline
				+ "maxLogQueueSize: " + maxLogQueueSize + newline
				+ "maxLogsPerSecond: " + maxLogsPerSecond + newline;
	}

	/**
	 * @return the centralizedLogger
	 */
	public String getCentralizedLogger() {
		return centralizedLogger;
	}

	/**
	 * @param centralizedLogger the centralizedLogger to set
	 */
	public void setCentralizedLogger(String centralizedLogger) {
		this.centralizedLogger = centralizedLogger;
	}

	/**
	 * @return the dispatchPacketSize
	 */
	public Integer getDispatchPacketSize() {
		return dispatchPacketSize;
	}

	/**
	 * @param dispatchPacketSize the dispatchPacketSize to set
	 */
	public void setDispatchPacketSize(Integer dispatchPacketSize) {
		this.dispatchPacketSize = dispatchPacketSize;
	}

	/**
	 * @return the flushPeriodSeconds
	 */
	public Integer getFlushPeriodSeconds() {
		return flushPeriodSeconds;
	}

	/**
	 * @param flushPeriodSeconds the flushPeriodSeconds to set
	 */
	public void setFlushPeriodSeconds(Integer flushPeriodSeconds) {
		this.flushPeriodSeconds = flushPeriodSeconds;
	}

	/**
	 * @return the immediateDispatchLevel
	 */
	public Integer getImmediateDispatchLevel() {
		return immediateDispatchLevel;
	}

	/**
	 * @param immediateDispatchLevel the immediateDispatchLevel to set
	 */
	public void setImmediateDispatchLevel(Integer immediateDispatchLevel) {
		this.immediateDispatchLevel = immediateDispatchLevel;
	}

	/**
	 * @return the maxLogQueueSize
	 */
	public Integer getMaxLogQueueSize() {
		return maxLogQueueSize;
	}

	/**
	 * @param maxLogQueueSize the maxLogQueueSize to set
	 */
	public void setMaxLogQueueSize(Integer maxLogQueueSize) {
		this.maxLogQueueSize = maxLogQueueSize;
	}
	
	/**
	 * @return the maxLogsPerSecond to allow
	 */
	public Integer getMaxLogsPerSecond() {
		return maxLogsPerSecond;
	}

	/**
	 * @param the maxLogsPerSecond to allow
	 */
	public void setMaxLogsPerSecond(Integer maxLogsPerSecond) {
		this.maxLogsPerSecond = maxLogsPerSecond;
	}

	/**
	 * @return the minLogLevelLocal
	 */
	public Integer getMinLogLevelLocal() {
		return minLogLevelLocal;
	}

	/**
	 * @param minLogLevelLocal the minLogLevelLocal to set
	 */
	public void setMinLogLevelLocal(Integer minLogLevelLocal) {
		this.minLogLevelLocal = minLogLevelLocal;
	}

	/**
	 * @return the minLogLevel
	 */
	public Integer getMinLogLevel() {
		return minLogLevel;
	}

	/**
	 * @param minLogLevel the minLogLevel to set
	 */
	public void setMinLogLevel(Integer minLogLevel) {
		this.minLogLevel = minLogLevel;
	}


    
}

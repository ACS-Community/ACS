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
 * File Manager.java
 */
package alma.TMCDB.maci;

@SuppressWarnings("serial")
public class Manager implements java.io.Serializable {
    //static private final String newline = System.getProperty("line.separator");

    @SuppressWarnings("unused")
	private int ManagerId;
    @SuppressWarnings("unused")
	private int ConfigurationId;
    
    // comma separated array
    private String Startup = "";
    private String ServiceComponents = "Log,LogFactory,NotifyEventChannelFactory,ArchivingChannel,LoggingChannel,InterfaceRepository,CDB,ACSLogSvc,PDB";
    private String ServiceDaemons = "";
    
    private LoggingConfig LoggingConfig;

    private double Timeout = 50.0;
    private double ClientPingInterval = 60;
    private double AdministratorPingInterval = 45;
    private double ContainerPingInterval = 30;

    private int ServerThreads = 10;
    
    // deperecated
    private String Execute = "";
    private String CommandLine = "";
    private double HeartbeatTimeout = 2.0;
    private int CacheSize = 10;
    private int MinCachePriority = 0;
    private int MaxCachePriority = 31;
    private String CentralizedLogger = "Log";
    
    
    /**
     * Default Constructor for Manager.  Setter methods must be used to insert data.
     */
    public Manager () {
    }

	/**
	 * @return the administratorPingInterval
	 */
	public double getAdministratorPingInterval() {
		return AdministratorPingInterval;
	}

	/**
	 * @param administratorPingInterval the administratorPingInterval to set
	 */
	public void setAdministratorPingInterval(double administratorPingInterval) {
		AdministratorPingInterval = administratorPingInterval;
	}

	/**
	 * @return the clientPingInterval
	 */
	public double getClientPingInterval() {
		return ClientPingInterval;
	}

	/**
	 * @param clientPingInterval the clientPingInterval to set
	 */
	public void setClientPingInterval(double clientPingInterval) {
		ClientPingInterval = clientPingInterval;
	}

	/**
	 * @return the commandLine
	 */
	public String getCommandLine() {
		return CommandLine;
	}

	/**
	 * @param commandLine the commandLine to set
	 */
	public void setCommandLine(String commandLine) {
		CommandLine = commandLine;
	}

	/**
	 * @return the containerPingInterval
	 */
	public double getContainerPingInterval() {
		return ContainerPingInterval;
	}

	/**
	 * @param containerPingInterval the containerPingInterval to set
	 */
	public void setContainerPingInterval(double containerPingInterval) {
		ContainerPingInterval = containerPingInterval;
	}

	/**
	 * @return the execute
	 */
	public String getExecute() {
		return Execute;
	}

	/**
	 * @param execute the execute to set
	 */
	public void setExecute(String execute) {
		Execute = execute;
	}

	/**
	 * @return the heartbeatTimeout
	 */
	public double getHeartbeatTimeout() {
		return HeartbeatTimeout;
	}

	/**
	 * @param heartbeatTimeout the heartbeatTimeout to set
	 */
	public void setHeartbeatTimeout(double heartbeatTimeout) {
		HeartbeatTimeout = heartbeatTimeout;
	}

	/**
	 * @return the loggingConfig
	 */
	public LoggingConfig getLoggingConfig() {
		return LoggingConfig;
	}

	/**
	 * @param loggingConfig the loggingConfig to set
	 */
	public void setLoggingConfig(LoggingConfig loggingConfig) {
		LoggingConfig = loggingConfig;
	}

	/**
	 * @return the serverThreads
	 */
	public int getServerThreads() {
		return ServerThreads;
	}

	/**
	 * @param serverThreads the serverThreads to set
	 */
	public void setServerThreads(int serverThreads) {
		ServerThreads = serverThreads;
	}

	/**
	 * @return the serviceComponents
	 */
	public String getServiceComponents() {
		if (ServiceComponents == null) return "";	// Oracle empty/NULL string workaround
		return ServiceComponents;
	}

	/**
	 * @param serviceComponents the serviceComponents to set
	 */
	public void setServiceComponents(String serviceComponents) {
		ServiceComponents = serviceComponents;
	}

	/**
	 * @return the serviceDaemons
	 */
	public String getServiceDaemons() {
		if (ServiceDaemons == null) return "";	// Oracle empty/NULL string workaround
		return ServiceDaemons;
	}

	/**
	 * @param serviceDaemons the serviceDaemons to set
	 */
	public void setServiceDaemons(String serviceDaemons) {
		ServiceDaemons = serviceDaemons;
	}

	/**
	 * @return the startup
	 */
	public String getStartup() {
		if (Startup == null) return "";	// Oracle empty/NULL string workaround
		return Startup;
	}

	/**
	 * @param startup the startup to set
	 */
	public void setStartup(String startup) {
		Startup = startup;
	}

	/**
	 * @return the timeout
	 */
	public double getTimeout() {
		return Timeout;
	}

	/**
	 * @param timeout the timeout to set
	 */
	public void setTimeout(double timeout) {
		Timeout = timeout;
	}

	/**
	 * @return the cacheSize
	 */
	public int getCacheSize() {
		return CacheSize;
	}

	/**
	 * @param cacheSize the cacheSize to set
	 */
	public void setCacheSize(int cacheSize) {
		CacheSize = cacheSize;
	}

	/**
	 * @return the centralizedLogger
	 */
	public String getCentralizedLogger() {
		return CentralizedLogger;
	}

	/**
	 * @param centralizedLogger the centralizedLogger to set
	 */
	public void setCentralizedLogger(String centralizedLogger) {
		CentralizedLogger = centralizedLogger;
	}

	/**
	 * @return the maxCachePriority
	 */
	public int getMaxCachePriority() {
		return MaxCachePriority;
	}

	/**
	 * @param maxCachePriority the maxCachePriority to set
	 */
	public void setMaxCachePriority(int maxCachePriority) {
		MaxCachePriority = maxCachePriority;
	}

	/**
	 * @return the minCachePriority
	 */
	public int getMinCachePriority() {
		return MinCachePriority;
	}

	/**
	 * @param minCachePriority the minCachePriority to set
	 */
	public void setMinCachePriority(int minCachePriority) {
		MinCachePriority = minCachePriority;
	}

}
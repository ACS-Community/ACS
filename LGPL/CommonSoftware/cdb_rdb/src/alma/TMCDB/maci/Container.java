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
 * File Container.java
 */
package alma.TMCDB.maci;

@SuppressWarnings("serial")
public class Container extends ContainerNode {
    static private final String newline = System.getProperty("line.separator");

    @SuppressWarnings("unused")
	private int ContainerId;
    @SuppressWarnings("unused")
	private int ConfigurationId;

    public String Name;
    
    private String ImplLang;
    private Double Timeout;
    private Boolean UseIFR = false;
    private Integer ManagerRetry;
    private Integer ServerThreads;
    private Boolean Recovery;

    private String Autoload; // string packed array

    private LoggingConfig LoggingConfig;
    
    private DeployInfo DeployInfo;
    
    private Integer PingInterval;
    
    // hierarchical support
    // must be public to be accessible, but should not have getter to be come visible as node
    public String Path;

    /**
     * Default Constructor for Component.  Setter methods must be used to insert data.
     */
    public Container () {
    }

    public String toString() {
    	String s =  "Container:" + newline;

        s += "\tName: " + Name + newline;

        s += "\tImplLang: " + ImplLang + newline;

        s += "\tTimeout: " + Timeout + newline;

        s += "\tManagerRetry: " + ManagerRetry + newline;

        s += "\tServerThreads: " + ServerThreads + newline;

        s += "\tRecovery: " + Recovery + newline;

        s += "\tAutoload: " + Autoload + newline;

        s += "\tLoggingConfig: " + LoggingConfig + newline;

        s += "\tPingInterval: " + PingInterval + newline;

        s += "\tDeployInfo: " + DeployInfo + newline;

        return s;
    }

	/**
	 * @return the name
	 */
	public String getName() {
		return Name;
	}

	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		Name = name;
	}

	/**
	 * @return the autoload
	 */
	public String getAutoload() {
		return Autoload;
	}

	/**
	 * @param autoload the autoload to set
	 */
	public void setAutoload(String autoload) {
		Autoload = autoload;
	}

	/**
	 * @return the deployInfo
	 */
	public DeployInfo getDeployInfo() {
		return DeployInfo;
	}

	/**
	 * @param deployInfo the deployInfo to set
	 */
	public void setDeployInfo(DeployInfo deployInfo) {
		DeployInfo = deployInfo;
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
	 * @return the managerRetry
	 */
	public Integer getManagerRetry() {
		return ManagerRetry;
	}

	/**
	 * @param managerRetry the managerRetry to set
	 */
	public void setManagerRetry(Integer managerRetry) {
		ManagerRetry = managerRetry;
	}

	/**
	 * @return the recovery
	 */
	public Boolean isRecovery() {
		return Recovery;
	}

	/**
	 * @param recovery the recovery to set
	 */
	public void setRecovery(Boolean recovery) {
		Recovery = recovery;
	}

	/**
	 * @return the serverThreads
	 */
	public Integer getServerThreads() {
		return ServerThreads;
	}

	/**
	 * @param serverThreads the serverThreads to set
	 */
	public void setServerThreads(Integer serverThreads) {
		ServerThreads = serverThreads;
	}

	/**
	 * @return the timeout
	 */
	public Double getTimeout() {
		return Timeout;
	}

	/**
	 * @param timeout the timeout to set
	 */
	public void setTimeout(Double timeout) {
		Timeout = timeout;
	}

	/**
	 * @return the useIFR
	 */
	public Boolean isUseIFR() {
		return UseIFR;
	}

	/**
	 * @param useIFR the useIFR to set
	 */
	public void setUseIFR(Boolean useIFR) {
		UseIFR = useIFR;
	}

	/**
	 * @return the implLang
	 */
	public String getImplLang() {
		return ImplLang;
	}

	/**
	 * @param implLang the implLang to set
	 */
	public void setImplLang(String implLang) {
		ImplLang = implLang;
	}

	/**
	 * @return the pingInterval
	 */
	public Integer getPingInterval() {
		return PingInterval;
	}

	/**
	 * @param pingInterval the pingInterval to set
	 */
	public void setPingInterval(Integer pingInterval) {
		PingInterval = pingInterval;
	}

}

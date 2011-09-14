/*
 * Created on May 14, 2003
 *
 * To change this generated comment go to 
 * Window>Preferences>Java>Code Generation>Code Template
 */
package com.cosylab.acs.maci.test;

import org.omg.CORBA.Object;

import alma.ACS.CBDescIn;

import si.ijs.maci.AuthenticationData;
import si.ijs.maci.CBComponentInfo;
import si.ijs.maci.ClientType;
import si.ijs.maci.ContainerPOA;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ImplLangType;

import si.ijs.maci.LoggingConfigurablePackage.LogLevels;

/**
 * @author almamgr
 */
public class ContainerProxyImpl extends ContainerPOA {

	protected String name;
	
	/**
	 * 
	 */
	public ContainerProxyImpl( String name ) {
		super();
		this.name = name;
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ContainerOperations#activate_COB(int, long, java.lang.String, java.lang.String, java.lang.String)
	 */
	public ComponentInfo activate_component(int arg0, long executionId, String arg1, String arg2, String arg3) {
		return null;
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ContainerOperations#deactivate_component(int)
	 */
	public void deactivate_component(int arg0) {
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ContainerOperations#shutdown(int)
	 */
	public void shutdown(int arg0) {
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ContainerOperations#get_COB_info(int[])
	 */
	public ComponentInfo[] get_component_info(int[] arg0) {
		return null;
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ClientOperations#name()
	 */
	public String name() {
		return name;
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ClientOperations#disconnect()
	 */
	public void disconnect() {

	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ClientOperations#authenticate(long, java.lang.String)
	 */
	public AuthenticationData authenticate(long executionId, String arg0) {
		return new AuthenticationData("", ClientType.CONTAINER_TYPE, ImplLangType.JAVA, false, System.currentTimeMillis(), executionId);
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ClientOperations#message(short, java.lang.String)
	 */
	public void message(short arg0, String arg1) {
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ClientOperations#taggedmessage(short, java.lang.String)
	 */
	public void taggedmessage(short arg0, short arg1, String arg2) {
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ClientOperations#ping()
	 */
	public boolean ping() {
		return true;
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ClientOperations#components_available(si.ijs.maci.ComponentInfo[])
	 */
	public void components_available(ComponentInfo[] arg0) {

	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ClientOperations#components_unavailable(java.lang.String[])
	 */
	public void components_unavailable(String[] arg0) {
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ContainerOperations#restart_component(int)
	 */
	public Object restart_component(int arg0) {
		return null;
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ContainerOperations#set_component_shutdown_order(int[])
	 */
	public void set_component_shutdown_order(int[] h) {
	}

	/* (non-Javadoc)
	 * @see si.ijs.maci.ContainerOperations#activate_component_async(int, long, java.lang.String, java.lang.String, java.lang.String, si.ijs.maci.CBComponentInfo, alma.ACS.CBDescIn)
	 */
	public void activate_component_async(int h, long execution_id, String name,
			String exe, String type, CBComponentInfo callback, CBDescIn desc) {
	}

	/************************** LoggingConfigurable *************************/

		/**
	 * Gets the log levels of the default logging configuration.
	 * These levels are used by all loggers that have not been configured individually.
	 */
        public LogLevels get_default_logLevels()
	{
	    throw new org.omg.CORBA.NO_IMPLEMENT("Method not implemented yet");
	}
       
            /**
             * Sets the log levels of the default logging configuration.
             * These levels are used by all loggers that have not been configured individually.
             */
        public void set_default_logLevels(LogLevels levels)
	{
	    throw new org.omg.CORBA.NO_IMPLEMENT("Method not implemented yet");
	}
       
            /** 
             * Gets the names of all loggers, to allow configuring their levels individually.
             * The names are those that appear in the log records in the field "SourceObject".
             * This includes the container logger, ORB logger, component
             * loggers, and (only C++) GlobalLogger. 
             */
        public String[] get_logger_names()
	{
	    throw new org.omg.CORBA.NO_IMPLEMENT("Method not implemented yet");
	}
       
            /**
             * Gets log levels for a particular named logger.
             * If the returned field LogLevels.useDefault is true, then the
             * logger uses the default levels, see get_default_logLevels();       
             * otherwise the returned local and remote levels apply.
             */
        public LogLevels get_logLevels(String logger_name)
	{
	    throw new org.omg.CORBA.NO_IMPLEMENT("Method not implemented yet");
	}
       
            /**
             * Sets log levels for a particular named logger.
             * If levels.useDefault is true, then the logger will be reset to
             * using default levels;       
             * otherwise it will use the supplied
             * local and remote levels. 
             */
        public void set_logLevels(String logger_name, LogLevels levels)
	{
	    throw new org.omg.CORBA.NO_IMPLEMENT("Method not implemented yet");
	}
       
            /**
             * Commands the container or manager to read in again the logging
             * configuration from the CDB and to reconfigure the loggers
             * accordingly. 
             * This allows for persistent changes in the logging
             * configuration to become effective, and also for changes of more
             * advanced parameters. 
             */
        public void refresh_logging_config()
	{
	    throw new org.omg.CORBA.NO_IMPLEMENT("Method not implemented yet");
	}
        /************************** END LoggingConfigurable *************************/

}

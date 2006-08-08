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
package alma.acs.logging.config;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.cosylab.CDB.DALOperations;
import com.cosylab.CDB.RecordDoesNotExist;
import com.cosylab.CDB.XMLerror;

/**
 * Class that encapsulates all configuration sources (defaults, properties, CDB) for Java logging,
 * and supports runtime updates of logging configuration based on these sources.
 * 
 * @author hsommer
 */
public class LogConfig {

//    /** property file with default logger settings to be found on the classpath */
//    private static final String DEFAULT_LOG_PROPERTY_FILE = "almalogging.properties";
//
//    /** property whose value is the property file with user settings */
//    private static final String USER_LOG_PROPERTY_FILE_PROPERTY = "java.util.logging.config.file";

    
    /**
     * The logger for messages logged by this class.
     * Note that this field will be null at first, until the Logger gets provided, 
     * so better use {@link #log(Level, String, Throwable)} for safe access. 
     */
	private Logger logger;

//    private LogManager m_logManager;
    
    /**
     * The ACS CDB. This value is null unless it gets set in {@link #setCDB(DAL)}.
     */
    private DALOperations cdb;
    
    /** 
     * Path in the CDB to the container logging configuration, 
     * which is also used for component loggers as a default 
     */
    private String cdbContainerPath;
    
    /** 
     * key = component logger name, value = path in the CDB to logger configuration 
     */
    private Map<String, String> cdbComponentPaths;
    
    private LogConfigData defaultLogConfigData;

	/** 
     * key = component logger name, value = LogConfigData for the named logger 
     */
    private Map<String, LogConfigData> namedLogConfigData;
    
    private List<LogConfigSubscriber> subscriberList;


	
	public LogConfig() {
        cdbComponentPaths = new HashMap<String, String>();
        namedLogConfigData = new HashMap<String, LogConfigData>();
		subscriberList = new ArrayList<LogConfigSubscriber>();
        defaultLogConfigData = new LogConfigData();
//        m_logManager = LogManager.getLogManager();
	}
    
    
    /**
     * Sets the reference to the CDB, which will then be used for configuration.
     * Before this method is called, default values are used instead of CDB values.
     * <p>
     * Note that the reference is only set, but the CDB is not read automatically;
     * for this, call {@link #initialize()}.
     * @param cdb a reference to the CDB. Will be ignored if == null.
     */
    public void setCDB(DALOperations cdb) {
        if (cdb != null) {
            this.cdb = cdb;
        }
    }
    
    /**
     * Sets the path to the CDB's node that configures all loggers.
     * Currently this is the &lt;Container&gt; element, but in the future
     * we should have a &lt;Logging&gt; element as its child;
     * then the same XML type could be used to specifically configure component's logger, 
     * overwriting the default values.
     * TODO: get the schemas changed ACS-wide   
     * @param curl
     */
    public void setCDBContainerPath(String curl) {
        cdbContainerPath = curl;
    }
    
    public void setCDBComponentPath(String compLoggerName, String curl) {
        cdbComponentPaths.put(compLoggerName, curl);
    }
    
    
	/**
	 * Initializes the values based on CDB settings, logging properties, etc.
     * All subscribing classes are notified of the new configuration,
     * see {@link LogConfigSubscriber#configureLogging(LogConfig)}.
     * <p>
	 * This method can be called more than once: if some settings have changed,
     * should be read in, and the logging classes should be notified of these changes.
     * For example, the container could call this method when it gets notified 
     * that the logging configuration in the CDB has been changed at runtime.
     * 
     * @throws LogConfigException if reading the configuration data failed and thus default values were used, 
     *                            or if there were problems during configuration even though some 
     *                            of the configuring went ok (best-effort approach).
	 */
	public void initialize() throws LogConfigException {
        StringBuffer errMsg = new StringBuffer();
        
//        setDefaultLogConfiguration();
//        setUserLogConfiguration();        
        String containerConfigXML = null;
        if (cdb != null) {
            if (cdbContainerPath != null) {
                try {
                    containerConfigXML = cdb.get_DAO(cdbContainerPath);
                    defaultLogConfigData.takeCdbContainerXml(containerConfigXML);
                } catch (LogConfigException ex) {
                    errMsg.append(ex.getMessage());
                } catch (Throwable thr) { // XMLerror, RecordDoesNotExist, etc
                    errMsg.append("Failed to read node " + cdbContainerPath + " from the CDB (msg='" + thr.getMessage() + "'). ");
                }
            }
            else {
                errMsg.append("CDB reference was set, but not the path to the container logging configuration. ");
            }
            
            namedLogConfigData.clear();
            // parse component logger configs and thus populate namedLogConfigData
            for (Iterator<String> iter = cdbComponentPaths.keySet().iterator(); iter.hasNext();) {                    
                String loggerName = iter.next();
                try {
                    getLogConfigData(loggerName);
                } catch (LogConfigException ex) {
                    errMsg.append(ex.getMessage());
                } catch (Throwable thr) { // XMLerror, RecordDoesNotExist, etc
                    errMsg.append("Failed to read config for logger " + loggerName + " from the CDB (msg='" + thr.getMessage() + "'). ");
                }
            }
        }

        notifySubscribers();
        
        // now that the subscribers had a chance to adjust their log levels according to the changes from the CDB (if present), we can publish a trace log
    	if (containerConfigXML != null) {
    		log(Level.FINER, "Updated default logging configuration based on CDB settings " + containerConfigXML, null);
    		// TODO: also log something for named loggers
    	}
    	else {
    		log(Level.FINER, "LogConfig was initialized, but not from CDB settings.", null);
    	}
        
        if (errMsg.length() > 0) {
            throw new LogConfigException("Log config initialization at least partially failed. " + errMsg.toString());
        }        
	}
	
    /**
     * Gets the default logging configuration data.
     * <p> 
     * Note that a copy of the config data is returned, so changes to it 
     * will not affect any other object's configuration.
     */
    public LogConfigData getLogConfigData() {
        return new LogConfigData(defaultLogConfigData);
    }

    /**
     * Gets the specialized logging configuration data for a given logger.
     * Resorts to the default configuration if a specialized configuration is not available.
     * <p> 
     * Note that a copy of the config data is returned, so changes to it 
     * will not affect any other object's configuration.
     * @throws RecordDoesNotExist 
     * @throws XMLerror 
     * @throws LogConfigException 
     */
    public LogConfigData getLogConfigData(String loggerName) throws XMLerror, RecordDoesNotExist, LogConfigException {
        if (loggerName == null) {
            return getLogConfigData();
        }
        LogConfigData ret = namedLogConfigData.get(loggerName);
        if (ret == null) {
            // in any case we can use the default config
            ret = getLogConfigData();
            // check if we have a CDB path for this particular logger which should modify the default settings
            String cdbPath = cdbComponentPaths.get(loggerName);
            if (cdbPath != null) {
                // CDB should have specialized config for this logger
                String componentConfigXML = cdb.get_DAO(cdbPath);
                ret.takeCdbComponentXml(componentConfigXML);
                setLogConfigData(loggerName, ret);
            }
        }
        return ret;
    }
    
    /**
     * Sets the logger configuration for a named logger.
     * <p>
     * From outside of this class, this method should be used only for tests and temporary hacks, 
     * as the {@link LogConfigData} is supposed to be created on demand in {@link #getLogConfigData(String)} 
     * where it should modify itself based on the CDB readings.
     *  
     * @param logConfigData
     */
    public void setLogConfigData(String loggerName, LogConfigData logConfigData) {
    	namedLogConfigData.put(loggerName, logConfigData);
    }
    
    
    /**
     * Sets the Logger to be used by this class and dependent classes for internal tracing.
     * <p>
     * Note that in the current design of ClientLogManager and LogConfig, 
     * the Logger can not be provided already in the constructor, 
     * because the Logger first must be configured, which in turn requires a LogConfig instance.
     * That's why we have this setter method.
     */
    public void setInternalLogger(Logger logger) {
    	this.logger = logger;
    	defaultLogConfigData.setInternalLogger(logger);
    	for (Iterator<LogConfigData> iter = namedLogConfigData.values().iterator(); iter.hasNext();) {
			iter.next().setInternalLogger(logger);			
		}
    }
    
    protected void log(Level level, String msg, Throwable thr) {
    	if (logger != null) {
    		logger.log(level, msg, thr);
    	}
    	else {    		
    		System.out.println(level.toString() + ": " + msg + (thr != null ? thr.toString() : ""));
    	}
    }
    
    /////////////////////////////////////////////////////////////////////
    // configuration based on JDK logging property files
    /////////////////////////////////////////////////////////////////////
    
//    /**
//     * Sets the default logging configuration.
//     * It is taken from the file almalogging.properties that comes with ACS.
//     */
//    private void setDefaultLogConfiguration()
//    {
//        InputStream is = null;
//
//        try
//        {
//            // Matej: Here we use getClass().getResourceAsStream() and
//            // NOT  ClassLoader.getSystemResourceAsStream()
//            // This method is not compatible with WebStart
//            // '/' means from the root (e.g. jar file)
//            is = getClass().getResourceAsStream("/"+DEFAULT_LOG_PROPERTY_FILE);
//            m_logManager.readConfiguration(is);
//        }
//        catch (Exception e)
//        {
//                System.err.println("failed to read default log configuration");
//                e.printStackTrace(System.err);
//        }
//    }
//    
//    /**
//     * Sets the user-specific logging configuration.
//     */
//    void setUserLogConfiguration()
//    {
//        String userConfigFile = System.getProperty(USER_LOG_PROPERTY_FILE_PROPERTY);
//        if (userConfigFile != null)
//        {
//            try
//            {
//                InputStream in = new FileInputStream(userConfigFile);
//                BufferedInputStream bin = new BufferedInputStream(in);
//                m_logManager.readConfiguration(bin);
////              System.out.println("configured logging from user-supplied properties file " + userConfigFile);
//            }
//            catch (Exception e)
//            {
//                System.err.println("failed to read user log configuration file '" + userConfigFile + "': ");
//                e.printStackTrace(System.err);
//            }
//        }
//    }
//    
//    /**
//     * Method getLoggerLevel. Needed for setting the level of each logger
//     * if it has been defined in the properties file.
//     * @param ns namespace
//     * @return Level
//     */
//    public Level getLoggerLevel(String ns)
//    {
//        String lev = m_logManager.getProperty(ns + ".level");
//        if (lev == null)
//        {
//            return Level.ALL;
//        }
//        
//        if (lev.indexOf(".") == -1)
//        {
//            String startName = lev.substring(0, 1);
//            String name = startName + lev.substring(1).toUpperCase();
//            return Level.parse(name);
//        }
//        else if (lev.startsWith("Level."))
//        {
//            int start = lev.indexOf('.');
//            String lvl = lev.substring(start);
//            String name = lvl.substring(1).toUpperCase();
//            return Level.parse(name);
//        }
//        else
//        {
//            System.err.println("Please set the logger's level according to the Java Logging API!");
//            return Level.parse("OFF");
//        }
//    }
//    
    
    
    /////////////////////////////////////////////////////////////////////
    // Propagation of configuration updates to various logging classes
    /////////////////////////////////////////////////////////////////////
    
	public void addSubscriber(LogConfigSubscriber subscriber) {
        if (!subscriberList.contains(subscriber)) {
            subscriberList.add(subscriber);
        }
	}
	
	void notifySubscribers() {
		for (Iterator<LogConfigSubscriber> iter = subscriberList.iterator(); iter.hasNext();) {
			LogConfigSubscriber subscriber = iter.next();
			subscriber.configureLogging(this);
		}
	}

    public void removeSubscriber(LogConfigSubscriber subscriber) {
        subscriberList.remove(subscriber);
    }

}

/*
 * LogFactory.java
 *
 * Created on May 8, 2003, 3:40 PM
 */

package cern.laser.guiplatform.util;

import java.net.URL;

import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;

/**
 * This is logger factory. All clasess inside cern.laser.guiplatform should use 
 * this class to obtain appropriate logger. 
 * This class is used because of problem with Logger inside Netbeans (where put 
 * log4j.properties file)
 * This class creates logger and configures it programmatically.
 *
 * @author  pawlowsk
 */
/**
 * For logger to be configured log4j.configuration System property
 * has to be set to give the location (including path) of the configuration file
 * which can be included in jar and the location given as the relative package path,
 * e.g. resources/log4j.properties. 
 * If this property is not provided the default configuration file
 * resources/log4j.properties provided in this projects jar file is used. 
 * 
 * @author ihabjan - Igor Habjan, Cosylab
 */

public class LogFactory {

    /** console pattern layout */
    private final static String CONSOLE_PATTERN_LAYOUT = "%5p [%t] (%F:%L) - %m%n";
    /** file pattern layout */
    private final static String FILE_PATTERN_LAYOUT = 
            "%5p [%t] {%d{dd HH:mm:ss}} (%F:%L) - %m%n";
     
    /** log filename and path */
    //private final static String LOG_FILE_NAME = "/tmp/alarm_console.log";
    private final static String LOG_FILE_NAME = System.getProperty("netbeans.user") + "/alarm_console.log";
    
    /** Creates a new instance of LogFactory */
    public LogFactory() {
    }
    /** Retrieve logger by name */
    public static Logger getLogger(String name) {
    	Logger logger = Logger.getLogger(name);

    	String log4j = System.getProperty("log4j.configuration");
    	if(log4j == null || log4j.length() == 0) {
    		// log4j property not set ==> use default properties file from <jar>!/resources/log4j.properties
    		URL resURL = LogFactory.class.getClassLoader().getResource("resources/log4j.properties");
    		PropertyConfigurator.configure(resURL);
    	} else {
        	PropertyConfigurator.configure(log4j);
    	}
    	
    	// If all is configured creation confirmation when debugging is given
    	// otherwise errors will occure (log4j, FileNotFound, ...)
    	logger.debug("Logger created for " + name);
        return logger;
    }
    /**
     * Help method which prints to logger stack trace of exception
     */
    public static void logException( Logger thisLogger, Exception exception ) {
        if ( thisLogger==null || exception==null)
            return;
        StackTraceElement [] stack = exception.getStackTrace();
        for(int i=0; i<stack.length; i++) 
            thisLogger.debug(stack[i]);           
    }
}

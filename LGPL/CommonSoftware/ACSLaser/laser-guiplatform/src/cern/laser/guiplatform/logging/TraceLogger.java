/*
 * TraceLogger.java
 *
 * Created on April 7, 2003, 5:30 PM
 */

package cern.laser.guiplatform.logging;

import org.apache.log4j.Logger;

/**
 * This class traces what user is doing, i. e. login/logout, etc
 * 
 * @author  pawlowsk
 */
public class TraceLogger {
    
    /** logger */
    private static Logger logger = Logger.getLogger(TraceLogger.class.getName());
    
    /** file pattern layout */
    private final static String FILE_PATTERN_LAYOUT = 
            "%5p [%t] {%d{dd HH:mm:ss}} (%F:%L) - %m%n";
     
    /** log filename and path */
    private final static String LOG_FILE_NAME = System.getProperty("netbeans.user") + "/alarm_console.log";
    
    // here should be logger configuration
    /*
    static {
        // configure logger programmatically
        logger.removeAllAppenders();
        //logger.addAppender(new ConsoleAppender(new PatternLayout(CONSOLE_PATTERN_LAYOUT)));
        try {
            logger.addAppender(
                new RollingFileAppender(new PatternLayout(FILE_PATTERN_LAYOUT),
                    LOG_FILE_NAME, true));
        } catch (java.io.IOException ioe) {
            //ioe.printStackTrace();
            throw new RuntimeException(ioe.toString());
        }
    }
    */
    /** Creates a new instance of TraceLogger */
    private TraceLogger() {
    }
    
    
    
    /** */
    public static void log(Object obj) {
       logger.debug(obj); //marek
    }
    
}

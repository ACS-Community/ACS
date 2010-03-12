package alma.acs.logging;

import java.util.logging.Logger;

import alma.acs.logging.AcsLogLevel;

/**
 * The ACS Logger outputs just the component name and the log message.
 * This can be a problem when a component is composed by more than one class.
 * This class prefixes the log message with the class name, the instance name
 * and the method name.
 * <p>
 * In practice I only use this verbosity in "developer" logs, i.e., those logged
 * in leves FINE, FINER and FINEST.
 */
public class VerboseLogger {

    /** Internal logger */
    private Logger logger;
    
    /** Class name */
    private String cls;
    
    public VerboseLogger(Logger logger, String className) {
        this.logger = logger;
        this.cls = className;
    }

    public Logger getInternalLogger() {
        return logger;
    }
    
    public void info(String msg) {
        logger.info(msg);
    }
    
    public void config(String msg) {
        logger.config(msg);
    }
    
    public void warning(String msg) {
        logger.severe(msg);
    }
    
    public void severe(String msg) {
        logger.severe(msg);
    }
    
    public void fine(String msg) {
        logger.log(AcsLogLevel.DEBUG, msg);
    }
    
    public void finer(String msg) {
        logger.log(AcsLogLevel.DEBUG, msg);
    }
    
    public void finest(String msg) {
        logger.log(AcsLogLevel.DEBUG, msg);
    }
    
    public void fine(String instance, String method, String msg) {
        fine(createLogPrefix(instance, method)+msg);        
    }

    public void fine(String method, String msg) {
        fine(createLogPrefix(method)+msg);        
    }
    
    public void finer(String instance, String method, String msg) {
        finer(createLogPrefix(instance, method)+msg);        
    }

    public void finer(String method, String msg) {
        fine(createLogPrefix(method)+msg);        
    }
    
    public void finest(String instance, String method, String msg) {
        fine(createLogPrefix(instance, method)+msg);        
    }

    public void finest(String method, String msg) {
        fine(createLogPrefix(method)+msg);        
    }
    
    private String createLogPrefix(String instance, String method) {
        return "{"+instance+"//"+cls+"::"+method+"} ";
    }
    
    private String createLogPrefix(String method) {
        return "{"+cls+"::"+method+"} ";
    }
}

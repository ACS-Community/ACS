/*
 * $Id: NetBeansLog.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.3 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */

package cern.gp.logging;

import java.io.InputStream;
import java.lang.reflect.Method;
import java.security.AccessControlException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Enumeration;
import java.util.Properties;

import cern.gp.util.GPManager;

import org.apache.commons.logging.Log;
import org.openide.ErrorManager;

/**
 * <p>NetBeans implementation of Log that uses the default ErrorManager.
 * The following system properties are supported to configure the behavior of this logger:</p>
 * <ul>
 * <li><code>{@link #LOG_LEVEL_PROPERTY}</code> - Default logging detail level for all instances of NetBeansLog.
 * Must be one of ("trace", "debug", "info", "warn", "error", or "fatal"). If not specified, defaults to
 * {@link #LOG_LEVEL_DEFAULT}.</li>
 * <li><code>{@link #SHOW_LOG_NAME_PROPERTY}</code> - Set to <code>true</code> if you want the Log instance name to be
 * included in output messages. Defaults to {@link #SHOW_LOG_NAME_DEFAULT}</li>
 * <li><code>{@link #SHOW_SHORT_LOG_NAME_PROPERTY}</code> - Set to <code>true</code> if you want the last component of
 * the name to be included in output messages. Defaults to {@link #SHOW_SHORT_LOG_NAME_DEFAULT}.</li>
 * <li><code>{@link #SHOW_DATE_TIME_PROPERTY}</code> - Set to <code>true</code> if you want the current date and time to
 * be included in output messages. Default is {@link #SHOW_DATE_TIME_DEFAULT}.</li>
 * <li><code>{@link #SYSTEM_PREFIX}.log.xxxxx</code> - Logging detail level for a NetBeansLog instance named "xxxxx".
 * Must be one of ("trace", "debug", "info", "warn", "error", or "fatal"). If not specified, the default
 * logging detail level is used.</li>
 * </ul>
 *
 * <p>In addition to looking for system properties with the names specified above, this implementation also checks for a
 * class loader resource named <code>{@link #LOG_PROPERTIES}</code>, and includes any matching definitions from this
 * resource (if it exists).
 * </p>
 *
 * @author Katarina Sigerud
 *
 * @version $Id: NetBeansLog.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 */
public class NetBeansLog implements Log {

  //
  // -- CLASS ATTRIBUTES ---------------------------------------------
  //

  //
  // -- LOG PROPERTY CONSTANTS ---------------------------------------
  //

  public static final String SYSTEM_PREFIX = "cern.gp.logging";
  public static final String LOG_LEVEL_PROPERTY = SYSTEM_PREFIX + ".DefaultLevel";
  public static final String SHOW_LOG_NAME_PROPERTY = SYSTEM_PREFIX + ".ShowLogName";
  public static final String SHOW_SHORT_LOG_NAME_PROPERTY = SYSTEM_PREFIX + ".ShowShortLogName";
  public static final String SHOW_DATE_TIME_PROPERTY = SYSTEM_PREFIX + ".ShowDateTime";
  public static final String DATE_FORMAT_PROPERTY = SYSTEM_PREFIX + ".DateFormat";

  //public static final String LOG_PROPERTIES = "nblog.properties";
  public static final String DATE_FORMAT_DEFAULT = "yyyy/MM/dd HH:mm:ss:SSS zzz";
  public static final String LOG_LEVEL_DEFAULT = "info";
  public static final boolean SHOW_LOG_NAME_DEFAULT = false;
  public static final boolean SHOW_SHORT_LOG_NAME_DEFAULT = true;
  public static final boolean SHOW_DATE_TIME_DEFAULT = false;

  /* Default ErrorManager */
  static protected final ErrorManager errMgr = ErrorManager.getDefault();
  /* All system properties which start with {@link #SYSTEM_PREFIX} */
  static protected final Properties nblogProps = new Properties();
  /* Include the instance name in the log message? */
  static protected boolean showLogName = SHOW_LOG_NAME_DEFAULT;
  /* Include the short name ( last component ) of the logger in the log message. 
   * Default to true - otherwise we'll be lost in a flood of messages without knowing who sends them. 
   */
  static protected boolean showShortName = SHOW_SHORT_LOG_NAME_DEFAULT;
  /* Include the current time in the log message. */
  static protected boolean showDateTime = SHOW_DATE_TIME_DEFAULT;
  /* Used to format times */
  static protected DateFormat dateFormatter = null;

  //
  // -- LOG LEVEL CONSTANTS ------------------------------------------
  //

  /** "Finest" level logging (<code>Debug window</code>). */
  public static final int LOG_LEVEL_TRACE = ErrorManager.INFORMATIONAL;
  /** "Finer" level logging (<code>ErrorManager.INFORMATIONAL + 1</code>) */
  public static final int LOG_LEVEL_DEBUG = ErrorManager.INFORMATIONAL + 1;
  /** "Config" level logging (<code>ErrorManager.WARNING</code>). */
  public static final int LOG_LEVEL_INFO = ErrorManager.USER;
  /** "Info" level logging (<code>ErrorManager.USER</code>) */
  public static final int LOG_LEVEL_WARN = ErrorManager.WARNING;
  /** "Warning" level logging = <code>ErrorManager.EXCEPTION</code> */
  public static final int LOG_LEVEL_ERROR = ErrorManager.EXCEPTION;
  /** "Severe" level logging = <code>ErrorManager.ERROR</code> */
  public static final int LOG_LEVEL_FATAL = ErrorManager.ERROR;

  /** Enable all logging levels */
  public static final int LOG_LEVEL_ALL = (LOG_LEVEL_TRACE - 1);
  /** Disable all logging levels */
  public static final int LOG_LEVEL_OFF = (LOG_LEVEL_FATAL + 1);

  //
  // -- INITIALIZER ---------------------------------------------
  //

  // initialize class attributes
  static {
    try {
      // add all system props that start with the specified prefix
      Enumeration enumer = System.getProperties().propertyNames();
      while (enumer.hasMoreElements()) {
        String name = (String) (enumer.nextElement());
        if (null != name && name.startsWith(SYSTEM_PREFIX)) {
          nblogProps.setProperty(name, System.getProperty(name));
        }
      }

      // identify the class loader to attempt resource loading with
      ClassLoader classLoader = null;
      try {
        Method method = Thread.class.getMethod("getContextClassLoader", null);
        classLoader = (ClassLoader) method.invoke(Thread.currentThread(), null);
      } catch (Exception e) {
        e.printStackTrace(); // Ignored (security exception or JDK 1.1)
      }
      if (classLoader == null) {
        classLoader = NetBeansLog.class.getClassLoader();
      }

      // add props from the resource nblog.properties
      //InputStream in = null;
      //in = NetBeansLog.class.getClassLoader().getResourceAsStream(LOG_PROPERTIES);

      //InputStream in = classLoader.getResourceAsStream("nblog.properties");
      //if (in != null) {
      //  try {
      //    nblogProps.load(in);
      //    in.close();
      // } catch (java.io.IOException e) {
      //    e.printStackTrace(); // ignored
      //  }
      //}

      /* That's a strange way to set properties. If the property
             is not set, we'll override the default
       
              showLogName = "true".equalsIgnoreCase(
                      nblogProps.getProperty(
                      SYSTEM_PREFIX + "showlogname","true"));
       */

      String prop = nblogProps.getProperty(SHOW_LOG_NAME_PROPERTY);
      if (prop != null)
        showLogName = "true".equalsIgnoreCase(prop);

      prop = nblogProps.getProperty(SHOW_SHORT_LOG_NAME_PROPERTY);
      if (prop != null) {
        showShortName = "true".equalsIgnoreCase(prop);
      }

      prop = nblogProps.getProperty(SHOW_DATE_TIME_PROPERTY);
      if (prop != null) {
        showDateTime = "true".equalsIgnoreCase(prop);
      }

      if (showDateTime) {
        dateFormatter = new SimpleDateFormat(nblogProps.getProperty(DATE_FORMAT_PROPERTY, DATE_FORMAT_DEFAULT));
      }
    } catch (AccessControlException e) {
      e.printStackTrace(); // ignore access control exceptions when trying to check system properties
    }
  }

  //
  // -- PROTECTED ATTRIBUTES ---------------------------------------------
  //

  /** The name of this simple log instance */
  protected String logName = null;
  /** The current log level */
  protected int currentLogLevel;

  private String prefix = null;

  //
  // -- CONSTRUCTOR ---------------------------------------------
  //

  /**
   * Construct a simple log with given name.
   * @param name log name
   */
  public NetBeansLog(String name) {
    logName = name;

    // set initial log level
    setLevel(NetBeansLog.LOG_LEVEL_WARN);

    // set log level from properties
    String lvl = nblogProps.getProperty(SYSTEM_PREFIX + ".log." + logName);
    int i = String.valueOf(name).lastIndexOf(".");
    while (null == lvl && i > -1) {
      name = name.substring(0, i);
      lvl = nblogProps.getProperty(SYSTEM_PREFIX + ".log." + name);
      i = String.valueOf(name).lastIndexOf(".");
    }

    if (null == lvl) {
      lvl = nblogProps.getProperty(LOG_LEVEL_PROPERTY);
    }

    if ("all".equalsIgnoreCase(lvl)) {
      setLevel(NetBeansLog.LOG_LEVEL_ALL);
    } else if ("trace".equalsIgnoreCase(lvl)) {
      setLevel(NetBeansLog.LOG_LEVEL_TRACE);
    } else if ("debug".equalsIgnoreCase(lvl)) {
      setLevel(NetBeansLog.LOG_LEVEL_DEBUG);
    } else if ("info".equalsIgnoreCase(lvl)) {
      setLevel(NetBeansLog.LOG_LEVEL_INFO);
    } else if ("warn".equalsIgnoreCase(lvl)) {
      setLevel(NetBeansLog.LOG_LEVEL_WARN);
    } else if ("error".equalsIgnoreCase(lvl)) {
      setLevel(NetBeansLog.LOG_LEVEL_ERROR);
    } else if ("fatal".equalsIgnoreCase(lvl)) {
      setLevel(NetBeansLog.LOG_LEVEL_FATAL);
    } else if ("off".equalsIgnoreCase(lvl)) {
      setLevel(NetBeansLog.LOG_LEVEL_OFF);
    }

  }

  //
  // -- PUBLIC METHODS ---------------------------------------------
  //

  /**
   * Set logging level. </p>
   * @param level new logging level
   */
  public void setLevel(int currentLogLevel) {

    this.currentLogLevel = currentLogLevel;

  }

  /**
   * Get logging level. </p>
   */
  public int getLevel() {

    return currentLogLevel;
  }

  //
  // -- implements Log ---------------------------------------------
  //

  /* (non-Javadoc)
   * @see org.apache.commons.logging.Log#isTraceEnabled()
   */
  public boolean isTraceEnabled() {
    return isLevelEnabled(NetBeansLog.LOG_LEVEL_TRACE);
  }

  /* (non-Javadoc)
   * @see org.apache.commons.logging.Log#isDebugEnabled()
   */
  public boolean isDebugEnabled() {
    return isLevelEnabled(NetBeansLog.LOG_LEVEL_DEBUG);
  }

  /* (non-Javadoc)
   * @see org.apache.commons.logging.Log#isInfoEnabled()
   */
  public boolean isInfoEnabled() {
    return isLevelEnabled(NetBeansLog.LOG_LEVEL_INFO);
  }

  /* (non-Javadoc)
   * @see org.apache.commons.logging.Log#isWarnEnabled()
   */
  public boolean isWarnEnabled() {
    return isLevelEnabled(NetBeansLog.LOG_LEVEL_WARN);
  }

  /* (non-Javadoc)
   * @see org.apache.commons.logging.Log#isErrorEnabled()
   */
  public boolean isErrorEnabled() {
    return isLevelEnabled(NetBeansLog.LOG_LEVEL_ERROR);
  }

  /* (non-Javadoc)
   * @see org.apache.commons.logging.Log#isFatalEnabled()
   */
  public boolean isFatalEnabled() {
    return isLevelEnabled(NetBeansLog.LOG_LEVEL_FATAL);
  }

  /* (non-Javadoc)
   * @see org.apache.commons.logging.Log#trace(java.lang.Object)
   */
  public void trace(Object message) {
    trace(message, null);
  }

  /* (non-Javadoc)
   * @see org.apache.commons.logging.Log#trace(java.lang.Object, java.lang.Throwable)
   */
  public void trace(Object message, Throwable t) {
    int logLevel = LOG_LEVEL_TRACE;
    if (isLevelEnabled(logLevel)) {
      display(logLevel, message, t);
      log(logLevel, message, t);
    }
  }

  /* (non-Javadoc)
   * @see org.apache.commons.logging.Log#debug(java.lang.Object)
   */
  public void debug(Object message) {
    debug(message, null);
  }

  /* (non-Javadoc)
   * @see org.apache.commons.logging.Log#debug(java.lang.Object, java.lang.Throwable)
   */
  public void debug(Object message, Throwable t) {
    int logLevel = LOG_LEVEL_DEBUG;
    if (isLevelEnabled(logLevel)) {
      display(logLevel, message, t);
      log(logLevel, message, t);
    }
  }

  /* (non-Javadoc)
   * @see org.apache.commons.logging.Log#info(java.lang.Object)
   */
  public void info(Object message) {
    info(message, null);
  }

  /* (non-Javadoc)
   * @see org.apache.commons.logging.Log#info(java.lang.Object, java.lang.Throwable)
   */
  public void info(Object message, Throwable t) {
    int logLevel = LOG_LEVEL_INFO;
    if (isLevelEnabled(logLevel)) {
      display(logLevel, message, t);
      log(logLevel, message, t);
    }
  }

  /* (non-Javadoc)
   * @see org.apache.commons.logging.Log#warn(java.lang.Object)
   */
  public void warn(Object message) {
    warn(message, null);
  }

  /* (non-Javadoc)
   * @see org.apache.commons.logging.Log#warn(java.lang.Object, java.lang.Throwable)
   */
  public void warn(Object message, Throwable t) {
    int logLevel = LOG_LEVEL_WARN;
    if (isLevelEnabled(logLevel)) {
      display(logLevel, message, t);
      log(logLevel, message, t);
    }
  }

  /* (non-Javadoc)
   * @see org.apache.commons.logging.Log#error(java.lang.Object)
   */
  public void error(Object message) {
    error(message, null); 
  }

  /* (non-Javadoc)
   * @see org.apache.commons.logging.Log#error(java.lang.Object, java.lang.Throwable)
   */
  public void error(Object message, Throwable t) {
    int logLevel = LOG_LEVEL_ERROR;
    if (isLevelEnabled(logLevel)) {
      display(logLevel, message, t);
      log(logLevel, message, t);
    }
  }

  /* (non-Javadoc)
   * @see org.apache.commons.logging.Log#fatal(java.lang.Object)
   */
  public void fatal(Object message) {
    fatal(message, null);
  }

  /* (non-Javadoc)
   * @see org.apache.commons.logging.Log#fatal(java.lang.Object, java.lang.Throwable)
   */
  public void fatal(Object message, Throwable t) {
    int logLevel = LOG_LEVEL_FATAL;
    if (isLevelEnabled(logLevel)) {
      display(logLevel, message, t);
      log(logLevel, message, t);
    }
  }

  //
  // -- PROTECTED METHODS ---------------------------------------------
  //

  protected void display(int logLevel, Object message, Throwable t) {
    String tStr = "";
    if (t != null) 
      tStr = " < " + t.getMessage() + " > ";
    //GPManager.getIO("LOG", false).getOut().println(createLogMessage(logLevel, message) + tStr);
  }

  /**
   * Do the actual logging. This method assembles the message and then logs using the default 
   * <code>ErrorManager</code>.
   */
  protected void log(int type, Object msg, Throwable t) {
    String message = createLogMessage(type, msg);

    if (t == null) {
      errMgr.log(type, message);
    } else {
      errMgr.notify(type, errMgr.annotate(t, message));
    }
  }

  protected String createLogMessage(int type, Object message) {
    // use a string buffer for better performance
    StringBuffer buf = new StringBuffer();
    
    // append date-time if so configured
    if (showDateTime) {
      buf.append(dateFormatter.format(new Date()));
      buf.append(" ");
    }
    
    // append a readable representation of the log leve
    switch (type) {
      case NetBeansLog.LOG_LEVEL_TRACE :
        buf.append("[TRACE] ");
        break;
      case NetBeansLog.LOG_LEVEL_DEBUG :
        buf.append("[DEBUG] ");
        break;
      case NetBeansLog.LOG_LEVEL_INFO :
        buf.append("[INFO] ");
        break;
      case NetBeansLog.LOG_LEVEL_WARN :
        buf.append("[WARN] ");
        break;
      case NetBeansLog.LOG_LEVEL_ERROR :
        buf.append("[ERROR] ");
        break;
      case NetBeansLog.LOG_LEVEL_FATAL :
        buf.append("[FATAL] ");
        break;
    }
    
    // append the name of the log instance if so configured
    if (showShortName) {
      if (prefix == null) {
        // cut all but the last component of the name for both styles
        prefix = logName.substring(logName.lastIndexOf(".") + 1) + " - ";
        prefix = prefix.substring(prefix.lastIndexOf("/") + 1) + "-";
      }
      buf.append(prefix);
    } else if (showLogName) {
      buf.append(String.valueOf(logName)).append(" - ");
    }
    
    // append the message
    buf.append(String.valueOf(message));
    
    // append stack trace if not null
//    if (t != null) {
//      buf.append(" <");
//      buf.append(t.toString());
//      buf.append(">");
//      t.printStackTrace();
//    }
    return buf.toString();
  }

  /**
   * Is the given log level currently enabled?
   *
   * @param logLevel is this level enabled?
   */
  protected boolean isLevelEnabled(int logLevel) {
    // log level are numerically ordered so can use simple numeric comparison
    return (logLevel >= currentLogLevel);
  }

}

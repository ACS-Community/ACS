package cern.cmw.mom.util;

import org.apache.log4j.Category;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringWriter;

import java.net.URL;

import java.util.Properties;


/**
 * Service class. Provides property definitions and initialization facility.
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 */
final public class MomConfig {
  static private Properties cmwProperties = null;

  /** Field CONFIG_FILE_LOCATION_PROPERTY           */
  public static final String CONFIGURATION_PROPERTY = "cmw.mom.config";

  /** Field CONFIGURATION_PROPERTY_FILE           */
  public static final String CONFIGURATION_PROPERTY_FILE = "cmw-mom-config.properties";

  /** Field CONFIGURATION_FILE           */
  public static final String CONFIGURATION_FILE = "cmw-mom.properties";

  /** Field CONNECTION_RETRY_PERIOD_PROPERTY           */
  public static final String CONNECTION_RETRY_PERIOD_PROPERTY = "cmw.mom.retry";

  /** Field CONNECTION_RETRY_NUMBER_PROPERTY           */
  public static final String CONNECTION_RETRY_NUMBER_PROPERTY = "cmw.mom.maxretry";

  /** Field CONNECTION_PING_INTERVAL_PROPERTY           */
  public static final String CONNECTION_PING_INTERVAL_PROPERTY = "cmw.mom.ping";

  /** Field USERNAME_PROPERTY           */
  public static final String USERNAME_PROPERTY = "cmw.mom.username";

  /** Field PASSWORD_PROPERTY           */
  public static final String PASSWORD_PROPERTY = "cmw.mom.password";

  /** Field BROKER_LIST_PROPERTY           */
  public static final String BROKER_LIST_PROPERTY = "cmw.mom.brokerlist";

  /** Field LOAD_BALANCING_PROPERTY           */
  public static final String LOAD_BALANCING_PROPERTY = "cmw.mom.loadbalancing";

  /** Field KEEP_ALIVE_PROPERTY           */
  public static final String KEEP_ALIVE_PROPERTY = "cmw.mom.keepalive";

  /** Field MSG_PERSISTANCE_PROPERTY           */
  public static final String MSG_PERSISTANCE_PROPERTY = "cmw.mom.persistance";

  /** Field MSG_PRIORITY_PROPERTY           */
  public static final String MSG_PRIORITY_PROPERTY = "cmw.mom.priority";

  /** Field MSG_TIMETOLIVE_PROPERTY           */
  public static final String MSG_TIMETOLIVE_PROPERTY = "cmw.mom.timetolive";

  /** Field SEQUENTIAL_PROPERTY           */
  public static final String SEQUENTIAL_PROPERTY = "cmw.mom.sequential";

  /** Field SELECTOR_AT_BROKER_PROPERTY           */
  public static final String SELECTOR_AT_BROKER_PROPERTY = "cmw.mom.selectoratbroker";

  /** Field NOTIFICATION_PROPERTY           */
  public static final String NOTIFICATION_PROPERTY = "cmw.mom.notification";
  
  private static final String DEFAULT_CONNECTION_RETRY_PERIOD = "10";
  private static final String DEFAULT_CONNECTION_RETRY_NUMBER = "99999";
  private static final String DEFAULT_CONNECTION_PING_INTERVAL = "5";
  private static final String DEFAULT_USERNAME = "cmw_usr";
  private static final String DEFAULT_PASSWORD = "cmw_pwd";
  private static final String DEFAULT_BROKER_LIST = "sljas1:2506";
  private static final String DEFAULT_LOAD_BALANCING = "false";
  private static final String DEFAULT_KEEP_ALIVE = "0";
  private static final String DEFAULT_MSG_PERSISTANCE = "false";
  private static final String DEFAULT_MSG_PRIORITY = "4";
  private static final String DEFAULT_MSG_TIMETOLIVE = "60000";
  private static final String DEFAULT_SEQUENTIAL = "true";
  private static final String DEFAULT_SELECTOR_AT_BROKER = "false";
  private static final String DEFAULT_NOTIFICATION = "false";
  static Category cat = Category.getInstance(MomConfig.class.getName());

  /**
   * Returns properties loaded from the CMW configuration file. The path of
   * the configuration file can be specified with the system property "cmw.config".
   * If this property is not specified, then the default will be used which
   * in the CERN environment is "http://slwww/~pca/cmw/cmw.cfg".
   * The order of priority is as follows : system properties, user defined config file,
   * default config file, default values.
   * @return java.util.Properties The CMW properties.
   */
  public synchronized static Properties getProperties(ClassLoader loader) {
    if (cmwProperties == null) {
      cat.info("loading configuration...");
      cmwProperties = new Properties();

      InputStream in_stream = null;

      try {
        // try to open the resource from a system property
        cat.debug("attempting to load configuration from system property...");
        in_stream = getInputStream(loader, System.getProperty(CONFIGURATION_PROPERTY));
      } catch (Exception e1) {
        cat.debug("failed : " + e1.getMessage());
      }

      if (in_stream == null) {
        cat.debug("failed");

        try {
          cat.debug("attempting to load configuration from default config file...");

          // try to open the default config file
          in_stream = getInputStream(loader, CONFIGURATION_FILE);
        } catch (Exception e2) {
          cat.debug("failed : " + e2.getMessage());
        }

        if (in_stream == null) {
          cat.debug("failed");

          try {
            // try to open the resource from a property file
            cat.debug("attempting to load configuration from property file...");

            InputStream property_stream = getInputStream(loader, CONFIGURATION_PROPERTY_FILE);
            Properties prop = new Properties();
            prop.load(property_stream);
            in_stream = getInputStream(loader, prop.getProperty(CONFIGURATION_PROPERTY));
          } catch (Exception e3) {
            cat.debug("failed : " + e3.getMessage());
          }
        }
      }

      if (in_stream != null) {
        try {
          BufferedInputStream bin_stream = new BufferedInputStream(in_stream);
          cmwProperties.load(bin_stream);
          in_stream.close();
          cat.info("configuration loaded");
        } catch (IOException ex) {
          cat.warn("could not load configuration, using defaults", ex);
        }
      } else {
        cat.warn("could not load configuration, using defaults");
      }

      setProperties();
    }

    return cmwProperties;
  }

  /**
   * Open the resource as Input Stream.
   */
  private static InputStream getInputStream(ClassLoader loader, String resource) throws IOException {
    InputStream in_stream = null;

    // Try to open a resource
    if (loader != null) {
      in_stream = loader.getResourceAsStream(resource);
    }

    if (in_stream == null) {
      // Try to open URL
      try {
        URL url = new URL(resource);
        in_stream = url.openStream();
      } catch (Exception e1) {
        // Try to open plain file
        try {
          in_stream = new FileInputStream(resource);
        } catch (Exception e2) {
          throw new IOException("unable to get the configuration");
        }
      }
    }

    return in_stream;
  }

  /**
   * Set the right values for the properties.
   */
  private static void setProperties() {
    cmwProperties.setProperty(CONNECTION_RETRY_PERIOD_PROPERTY, System.getProperty(CONNECTION_RETRY_PERIOD_PROPERTY, cmwProperties.getProperty(CONNECTION_RETRY_PERIOD_PROPERTY, DEFAULT_CONNECTION_RETRY_PERIOD)));
    cmwProperties.setProperty(CONNECTION_RETRY_NUMBER_PROPERTY, System.getProperty(CONNECTION_RETRY_NUMBER_PROPERTY, cmwProperties.getProperty(CONNECTION_RETRY_NUMBER_PROPERTY, DEFAULT_CONNECTION_RETRY_NUMBER)));
    cmwProperties.setProperty(CONNECTION_PING_INTERVAL_PROPERTY, System.getProperty(CONNECTION_PING_INTERVAL_PROPERTY, cmwProperties.getProperty(CONNECTION_PING_INTERVAL_PROPERTY, DEFAULT_CONNECTION_PING_INTERVAL)));
    cmwProperties.setProperty(USERNAME_PROPERTY, System.getProperty(USERNAME_PROPERTY, cmwProperties.getProperty(USERNAME_PROPERTY, DEFAULT_USERNAME)));
    cmwProperties.setProperty(PASSWORD_PROPERTY, System.getProperty(PASSWORD_PROPERTY, cmwProperties.getProperty(PASSWORD_PROPERTY, DEFAULT_PASSWORD)));
    cmwProperties.setProperty(BROKER_LIST_PROPERTY, System.getProperty(BROKER_LIST_PROPERTY, cmwProperties.getProperty(BROKER_LIST_PROPERTY, DEFAULT_BROKER_LIST)));
    cmwProperties.setProperty(LOAD_BALANCING_PROPERTY, System.getProperty(LOAD_BALANCING_PROPERTY, cmwProperties.getProperty(LOAD_BALANCING_PROPERTY, DEFAULT_LOAD_BALANCING)));
    cmwProperties.setProperty(KEEP_ALIVE_PROPERTY, System.getProperty(KEEP_ALIVE_PROPERTY, cmwProperties.getProperty(KEEP_ALIVE_PROPERTY, DEFAULT_KEEP_ALIVE)));
    cmwProperties.setProperty(MSG_PERSISTANCE_PROPERTY, System.getProperty(MSG_PERSISTANCE_PROPERTY, cmwProperties.getProperty(MSG_PERSISTANCE_PROPERTY, DEFAULT_MSG_PERSISTANCE)));
    cmwProperties.setProperty(MSG_PRIORITY_PROPERTY, System.getProperty(MSG_PRIORITY_PROPERTY, cmwProperties.getProperty(MSG_PRIORITY_PROPERTY, DEFAULT_MSG_PRIORITY)));
    cmwProperties.setProperty(MSG_TIMETOLIVE_PROPERTY, System.getProperty(MSG_TIMETOLIVE_PROPERTY, cmwProperties.getProperty(MSG_TIMETOLIVE_PROPERTY, DEFAULT_MSG_TIMETOLIVE)));
    cmwProperties.setProperty(SEQUENTIAL_PROPERTY, System.getProperty(SEQUENTIAL_PROPERTY, cmwProperties.getProperty(SEQUENTIAL_PROPERTY, DEFAULT_SEQUENTIAL)));
    cmwProperties.setProperty(SELECTOR_AT_BROKER_PROPERTY, System.getProperty(SELECTOR_AT_BROKER_PROPERTY, cmwProperties.getProperty(SELECTOR_AT_BROKER_PROPERTY, DEFAULT_SELECTOR_AT_BROKER)));
    cmwProperties.setProperty(NOTIFICATION_PROPERTY, System.getProperty(NOTIFICATION_PROPERTY, cmwProperties.getProperty(NOTIFICATION_PROPERTY, DEFAULT_NOTIFICATION)));

    StringWriter s_writer = new StringWriter();
    cmwProperties.list(new PrintWriter(s_writer));
    cat.debug("using properties :\n" + s_writer.toString());
  }
}


/*--- Formatted in Sun Java Convention Style on Mon, Feb 12, '01 ---*/
/*------ Formatted by Jindent 3.23 Gold 1.02 --- http://www.jindent.de ------*/

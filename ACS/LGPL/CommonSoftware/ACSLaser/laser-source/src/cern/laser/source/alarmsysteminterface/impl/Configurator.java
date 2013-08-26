/*
 * Configurator.java
 *
 * Created on February 27, 2003, 11:29 AM
 */
package cern.laser.source.alarmsysteminterface.impl;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.Properties;

import org.apache.log4j.Category;
import org.exolab.castor.xml.Unmarshaller;

import cern.laser.source.alarmsysteminterface.ASIException;
import cern.laser.source.alarmsysteminterface.impl.configuration.ASIConfiguration;


/**
 *
 * @author  fracalde
 */
public class Configurator {
  /** logging category
   */
  private static Category cat = Category.getInstance(Configurator.class.getName());

  /** configuration
   */
  private static final String CONFIGURATION_FILE = "asi-configuration.xml";

  /** configuration
   */
  private static final String CONFIGURATION_PROPERTY = "laser.asi.config";

  /** configuration
   */
  private static final String CONFIGURATION_PROPERTY_FILE = "asi-configuration.properties";

  /** configuration
   */
  private ASIConfiguration configuration = null;

  /** Creates a new instance of Configurator */
  public Configurator() {
  }

  /** Load the API configuration
   * @return the API configuration
   * @throws ASIException if unable to get the configuration
   */
  public ASIConfiguration getConfiguration() throws ASIException {
    if (configuration == null) {
      cat.info("loading configuration...");

      InputStream in_stream = null;

      try {
        // try to open the resource from a system property
        cat.debug("attempting to load configuration from system property...");
        in_stream = Configurator.class.getResourceAsStream(CONFIGURATION_FILE);
      } catch (Exception e1) {
        cat.debug("failed : " + e1.getMessage());
      }

      if (in_stream == null) {
        cat.debug("failed");

        try {
          // try to open the default config file
          in_stream = getInputStream(CONFIGURATION_FILE);
        } catch (Exception e2) {
          cat.debug("failed : " + e2.getMessage());
        }

        if (in_stream == null) {
          cat.debug("failed");

          try {
            // try to open the resource from a property file
            cat.debug("attempting to load configuration from property file...");

            // try to open the resource from a property file
            InputStream property_stream = getInputStream(CONFIGURATION_PROPERTY_FILE);
            Properties properties = new Properties();
            properties.load(property_stream);
            in_stream = getInputStream(properties.getProperty(CONFIGURATION_PROPERTY));
          } catch (Exception e3) {
            cat.debug("failed : " + e3.getMessage());
          }
        }
      }

      if (in_stream != null) {
        try {
          configuration = (ASIConfiguration) Unmarshaller.unmarshal(ASIConfiguration.class, new InputStreamReader(new BufferedInputStream(in_stream)));
        } catch (Exception e4) {
          cat.error("unable to load configuration", e4);
          throw new ASIException("unable to load configuration");
        }
      } else {
        cat.error("unable to load configuration");
        throw new ASIException("unable to load configuration");
      }
    }

    cat.info("configuration loaded");

    return configuration;
  }

  /** load a resource via an InputStream
   * @param resource the resource to load
   * @throw IOException if loading fails
   */
  private InputStream getInputStream(String resource) throws IOException {
    if (resource == null) {
      throw new IllegalArgumentException("resource name can not be null");
    }

    // Try to open a resource
    InputStream in_stream = this.getClass().getClassLoader().getResourceAsStream(resource);

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
          throw new IOException("unable to get the configuration : " + e2.getMessage());
        }
      }
    }

    return in_stream;
  }
}

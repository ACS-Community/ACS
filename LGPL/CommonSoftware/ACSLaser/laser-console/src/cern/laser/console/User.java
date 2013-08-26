/*
 * $Id: User.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.console;
import java.util.Collection;

import cern.laser.client.LaserConnectionException;

/** Alarm console user.
 * @author F.Calderini
 * @see cern.laser.console.Configuration
 */
public interface User 
{
  /** Accessor method.
   * @return the user name.
   * @throws LaserConsoleException if the request could not be served.
   */    
  public String getName() throws LaserConsoleException;
  /** Accessor method.
   * @return the user password.
   * @throws LaserConsoleException if the request could not be served.
   */    
  public String getPassword() throws LaserConsoleException;
  /** Accessor method.
   * @return newPassword the user password.
   * @throws LaserConsoleException if the request could not be served.
   */    
  public void setPassword(String newPassword) throws LaserConsoleException;
  
  public String getDefaultPrinter() throws LaserConsoleException;
  
  public void setDefaultPrinter(String newDefaultPrinter) throws LaserConsoleException;
  
  /** Accessor method.
   * @return the user console configurations.
   * @throws LaserConsoleException if the request could not be served.
   * @throws LaserConnectionException if the console cannot connect to the BL
   */    
  public Collection getConfigurations() throws LaserConsoleException, LaserConnectionException;
  /** Accessor method.
   * @return the user console configuration.
   * @param name the name of the configuration.
   * @throws LaserConnectionException if the console cannot connect to the BL
   * @throws LaserConfigurationNotFoundException if the configuration does not exist.
   */    
  public Configuration getConfiguration(String name) throws LaserConsoleException, LaserConnectionException;
  /** Create a new alarm console configuration for the user.
   * @param name the name of the configuration to create.
   * @return the configuration that has been created with default values.
   * @throws LaserConsoleException if the request could not be served.
   * @throws LaserConfigurationDuplicationException if the configuration already exists.
   */    
  public Configuration createConfiguration(String name) throws LaserConsoleException;
  /** Create a new alarm console configuration for the user using a given configuration.
   * @param configuration the configuration to be used to create the new one.
   * @return the configuration that has been created from the given configuration.
   * @throws LaserConsoleException if the request could not be served.
   * @throws LaserConfigurationDuplicationException if the configuration already exists.
   */    
  public Configuration createConfiguration(Configuration configuration) throws LaserConsoleException;
  /** Remove a configuration for the user.
   * @param name the name of the configuration to remove.
   * @throws LaserConsoleException if the request could not be served.
   * @throws LaserConfigurationNotFoundException if the configuration does not exist.
   */    
  public void removeConfiguration(String name) throws LaserConsoleException;
  /** Get the user's default configuration.
   * @return the user console default configuration, null if not set.
   * @throws LaserConsoleException if the request could not be served.
   * @throws LaserConnectionException if the console cannot connect to the BL
   */    
  public Configuration getDefaultConfiguration() throws LaserConsoleException, LaserConnectionException;
  /** Set the user's default configuration.
   * @param name the user's console new default configuration name.
   * @throws LaserConsoleException if the request could not be served.
   * @throws LaserConfigurationNotFoundException if the configuration does not exist.
   */    
  public void setDefaultConfiguration(String name) throws LaserConsoleException;
  /** Check the user default configuration.
   * @param name the configuration name.
   * @return true if the configuration is the user's default, false otherwise
   * @throws LaserConsoleException if the request could not be served.
   */    
  public boolean isDefaultConfiguration(String name) throws LaserConsoleException;
  
}
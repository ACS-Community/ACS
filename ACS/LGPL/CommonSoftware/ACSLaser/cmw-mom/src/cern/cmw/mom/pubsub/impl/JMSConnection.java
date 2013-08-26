package cern.cmw.mom.pubsub.impl;

import javax.jms.ExceptionListener;


/**
 * Interface class. Defines an interface to manage a generic JMS connection.
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 */
public interface JMSConnection {
  /**
   * Get the unique connection identifier associated with the connection.
   * @return String The connection identifier.
   */
  public String getConnectId();

  /**
   * Check if the connection is estabilished.
   * @return boolean true if the connection is active.
   */
  public boolean isConnected();

  /**
   * Set the listener for JMSException.
   * @param listener the ExceptionListener instance implementing the onException() method
   * @exception ConnectionException JMS was unable to set the ExceptionListener for the connection.
   */
  public void setExceptionListener(ExceptionListener listener) throws ConnectionException;

  /**
   * Get the username, if any, associated with the connection.
   * @return String The username.
   */
  public String getUsername();

  /**
   * Close the connection.
   */
  public void close();

  /**
   * Estabilish a connection to a JMS broker.
   * @param retry true if connection retry mechanism is requested.
   * @throws ConnectionException
   */
  public void connect(boolean retry) throws ConnectionException;

  /**
   * Disconnect from the JMS broker.
   */
  public void disconnect();

  /**
   * Start the connection.
   *
   * @throws ConnectionException
   */
  public void start() throws ConnectionException;

  /**
   * Stop the connection.
   *
   * @throws ConnectionException
   */
  public void stop() throws ConnectionException;
}


/*--- Formatted in Sun Java Convention Style on Tue, Jun 26, '01 ---*/
/*------ Formatted by Jindent 3.23 Gold 1.02 --- http://www.jindent.de ------*/

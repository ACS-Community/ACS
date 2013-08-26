package cern.cmw.mom.pubsub.impl;

import cern.cmw.mom.util.MomConfig;

import org.apache.log4j.Category;

import javax.jms.ExceptionListener;
import javax.jms.JMSException;
import javax.jms.TopicSession;


/**
 * Implementation class.
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 * @see JMSTopicConnection
 */
public class DefaultJMSTopicConnectionImpl implements JMSTopicConnection {
  static Category cat = Category.getInstance(DefaultJMSTopicConnectionImpl.class.getName());
  private java.util.Properties momProperties = null;
  private String password = null;
  private String username = null;
  //private progress.message.jclient.TopicConnection brokerConnection = null;
  private com.cosylab.acs.jms.ACSJMSTopicConnection brokerConnection = null;
  //private progress.message.jclient.TopicConnectionFactory factory = null;
  private com.cosylab.acs.jms.ACSJMSTopicConnectionFactory factory = null;
  private boolean connected = false;
  private int pingInterval = 0;
  private int retryInterval = 0;
  private int retryNumber = 0;

  /**
   * Constructor DefaultJMSTopicConnectionImpl
   *
   * @throws JMSException
   */
  protected DefaultJMSTopicConnectionImpl(String username, String password, String brokerList, Boolean loadBalancing, Boolean sequential, Boolean selectorAtBroker) throws JMSException {
    cat.debug("DefaultJMSTopicConnectionImpl()");
    momProperties = MomConfig.getProperties(this.getClass().getClassLoader());
    this.username = ((username != null) ? username : momProperties.getProperty(MomConfig.USERNAME_PROPERTY));
    this.password = ((password != null) ? password : momProperties.getProperty(MomConfig.PASSWORD_PROPERTY));
    retryInterval = Integer.parseInt(momProperties.getProperty(MomConfig.CONNECTION_RETRY_PERIOD_PROPERTY));
    retryNumber = Integer.parseInt(momProperties.getProperty(MomConfig.CONNECTION_RETRY_NUMBER_PROPERTY));
    pingInterval = Integer.parseInt(momProperties.getProperty(MomConfig.CONNECTION_PING_INTERVAL_PROPERTY));
    //factory = new progress.message.jclient.TopicConnectionFactory();
    factory = new com.cosylab.acs.jms.ACSJMSTopicConnectionFactory(null);
    //factory.setConnectionURLs((brokerList != null) ? brokerList : momProperties.getProperty(MomConfig.BROKER_LIST_PROPERTY));
    //factory.setSequentialBoolean((sequential != null) ? sequential : Boolean.valueOf(momProperties.getProperty(MomConfig.SEQUENTIAL_PROPERTY)));
    //factory.setLoadBalancingBoolean((loadBalancing != null) ? loadBalancing : Boolean.valueOf(momProperties.getProperty(MomConfig.LOAD_BALANCING_PROPERTY)));
    //factory.setSelectorAtBroker((selectorAtBroker != null) ? selectorAtBroker : Boolean.valueOf(momProperties.getProperty(MomConfig.SELECTOR_AT_BROKER_PROPERTY)));
  }

  /**
   * Method instance
   *
   * @param username the user name
   * @param password the password
   * @param brokerList the coma separated broker URLs list (in the form [protocol://]hostname[:port])
   * @param loadBalancing if true, indicates that the client is willing to have a connect request re-directed to another broker within a cluster
   * @param sequential if true, the broker list will be scanned sequentially
   * @param selectorAtBroker if true, selectors will be evaluated on the broker side
   * @return DefaultJMSTopicConnectionImpl
   *
   * @throws ConnectionException
   *
   */
  public static DefaultJMSTopicConnectionImpl instance(String username, String password, String brokerList, Boolean loadBalancing, Boolean sequential, Boolean selectorAtBroker) throws ConnectionException {
    cat.debug("instance()");

    DefaultJMSTopicConnectionImpl connection = null;

    try {
      connection = new DefaultJMSTopicConnectionImpl(username, password, brokerList, loadBalancing, sequential, selectorAtBroker);
    } catch (JMSException e) {
      throw (new ConnectionException(e.getMessage()));
    }

    return connection;
  }

  /**
   * Method getConnectId
   *
   *
   * @return String
   *
   */
  public String getConnectId() {
    String connectId = "";

    if (isConnected()) {
      //connectId = brokerConnection.getConnectID();
        try {
            connectId = brokerConnection.getClientID();
        } catch (javax.jms.JMSException e) {
            cat.error(("Error getting the client ID: "+e.toString()));
            return "";
        }
    }

    return connectId;
  }

  /**
   * Method isConnected
   *
   *
   * @return boolean
   *
   */
  public boolean isConnected() {
    return (connected);
  }

  /**
   * Method setExceptionListener
   *
   *
   * @param listener
   *
   * @throws ConnectionException
   *
   */
  public void setExceptionListener(ExceptionListener listener) throws ConnectionException {
    cat.debug("setExceptionListener()");

    if (isConnected()) {
      try {
        brokerConnection.setExceptionListener(listener);
      } catch (JMSException e) {
        throw (new ConnectionException(e.getMessage()));
      }
    } else {
      throw (new ConnectionException("Not connected!"));
    }
  }

  /**
   * Method getUsername
   *
   *
   * @return String
   *
   */
  public String getUsername() {
    return username;
  }

  /**
   * Method close
   *
   *
   */
  public void close() {
    cat.debug("close()");

    if (brokerConnection != null) {
      try {
        brokerConnection.close();
      } catch (JMSException e) {
        cat.warn(e.getMessage());
      }
    }

    disconnect();
  }

  /**
   * Method connect
   *
   *
   * @param retry
   * @throws ConnectionException
   */
  public void connect(boolean retry) throws ConnectionException {
    // retries == 1 means no retry (just one go)
    cat.debug("connect()");

    int max_retries = 1;

    if (retry) {
      max_retries = retryNumber;
    }

    int retry_num = 0;

    while ((!connected) && (retry_num < max_retries)) {
      if (retry) {
        // Wait before reconnection in retry mode
        try {
          Thread.sleep(retryInterval * 1000);
        } catch (InterruptedException ie) {
          cat.error(ie.getMessage());
        }
      }

      try {
          cat.info("Connecting...");
          //cat.info("Connecting " + username + " to " + factory.getConnectionURLs() + "...");
          //brokerConnection = (progress.message.jclient.TopicConnection) factory.createTopicConnection(username, password);
          brokerConnection = (com.cosylab.acs.jms.ACSJMSTopicConnection) factory.createTopicConnection(username, password);

        //brokerConnection = new progress.message.jclient.TopicConnection(DEFAULT_BROKER_NAME+":"+DEFAULT_BROKER_PORT, null, DEFAULT_USERNAME, DEFAULT_PASSWORD);
        connected = true;
        //cat.info("Connected to " + brokerConnection.getBrokerURL());
        cat.info("Connected ");
      } catch (javax.jms.JMSException em) {
        cat.warn("Connection refused", em);
      }

      retry_num++;
    }

    if (!connected) {
      throw (new ConnectionException("Unable to (re)estabilish a connection to the message broker!"));
    }

    // Ten seconds so it will ping six times a minute
    //brokerConnection.setPingInterval(pingInterval);
  }

  /**
   * Method createTopicSession
   *
   *
   * @return TopicSession
   *
   * @throws ConnectionException
   *
   */
  public TopicSession createTopicSession() throws ConnectionException {
    cat.debug("createTopicSession()");

    if (isConnected()) {
      try {
        return brokerConnection.createTopicSession(false, TopicSession.AUTO_ACKNOWLEDGE);
      } catch (JMSException e) {
        throw (new ConnectionException(e.getMessage()));
      }
    } else {
      throw (new ConnectionException("Not connected!"));
    }
  }

  /**
   * Method disconnect
   *
   *
   */
  public void disconnect() {
    cat.debug("disconnect()");

    // Cause the process to block indefinitevly
    if (isConnected()) {
      //try {stop();} catch (ConnectionException e) { cat.debug("Unable to stop the connection"); }
      //try {brokerConnection.close();} catch (JMSException e) { cat.debug("Unable to close the connection"); }
    }

    brokerConnection = null;
    connected = false;
    cat.info("disconnected");
  }

  /**
   * Method start
   *
   *
   * @throws ConnectionException
   *
   */
  public void start() throws ConnectionException {
    cat.debug("start()");

    if (isConnected()) {
      try {
        brokerConnection.start();
      } catch (JMSException e) {
        throw (new ConnectionException(e.getMessage()));
      }
    } else {
      throw (new ConnectionException("Not connected!"));
    }
  }

  /**
   * Method stop
   *
   *
   * @throws ConnectionException
   *
   */
  public void stop() throws ConnectionException {
    cat.debug("stop()");

    if (isConnected()) {
      try {
        brokerConnection.stop();
      } catch (JMSException e) {
        throw (new ConnectionException(e.getMessage()));
      }
    } else {
      throw (new ConnectionException("Not connected!"));
    }
  }

  /**
   * Method finalize
   *
   * @throws Throwable
   */
  protected void finalize() throws Throwable {
    cat.debug("finalize()");
    close();
    super.finalize();
  }
}


/*--- Formatted in Sun Java Convention Style on Fri, Jun 29, '01 ---*/
/*------ Formatted by Jindent 3.23 Gold 1.02 --- http://www.jindent.de ------*/

package cern.cmw.mom.pubsub.impl;


/**
 * Factory class. Creates instances of JMSTopicConnection.
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 * @see JMSTopicConnection
 */
public class JMSTopicConnectionFactory {
  /**
   * Factory method.
   * @param username the user name
   * @param password the password
   * @param brokerList the coma separated broker URLs list (in the form [protocol://]hostname[:port])
   * @param loadBalancing if true, indicates that the client is willing to have a connect request re-directed to another broker within the list
   * @param sequential if true, the broker list will be scanned sequentially
   * @param selectorAtBroker if true, selectors will be evaluated on the broker side
   * @return JMSTopicConnection the JMSTopicConnection instance.
   * @exception ConnectionException the system was unable to create the instance.
   */
  public static JMSTopicConnection createJMSTopicConnection(String username, String password, String brokerList, Boolean loadBalancing, Boolean sequential, Boolean selectorAtBroker) throws ConnectionException {
    return ACSJMSTopicConnectionImpl.instance(username, password, brokerList, loadBalancing, sequential, selectorAtBroker);
  }
}

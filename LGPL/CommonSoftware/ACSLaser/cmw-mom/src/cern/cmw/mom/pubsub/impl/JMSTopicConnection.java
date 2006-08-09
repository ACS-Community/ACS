package cern.cmw.mom.pubsub.impl;

import javax.jms.TopicSession;


/**
 * Interface class. Defines an interface to manage a JMS topic connection.
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 */
public interface JMSTopicConnection extends JMSConnection {
  /**
   * Create a topic session.
   * @return TopicSession The session.
   * @exception ConnectionException The system failed to create a session.
   */
  public TopicSession createTopicSession() throws ConnectionException;
}

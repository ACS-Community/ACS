package cern.cmw.mom.pubsub;

import cern.cmw.mom.pubsub.impl.DefaultPublisherImpl;
import cern.cmw.mom.pubsub.impl.DefaultSubscriberImpl;


/**
 * Public class. Factory class for creating Publisher or Subscriber instances.
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 * @see Subscriber
 * @see Publisher
 */
public final class PubSubFactory {
  /**
   * Create a new Publisher instance.
   *
   * @return the Publisher instance.
   * @exception MOMException if the system fails to instantiate the Publisher due to some internal error.
   */
  public static Publisher publisher() throws MOMException {
    return new DefaultPublisherImpl();
  }

  /**
   * Create a new Publisher instance using default properties.
   *
   * @param username the user name. This may be null.
   * @param password the password. This may be null.
   * @param brokerList the coma separated broker URLs list (in the form [protocol://]hostname[:port]). This may be null.
   * @param loadBalancing if true, indicates that the client is willing to have a connect request re-directed to another broker within the broker list. This may be null.
   * @param sequential if true, the broker list will be scanned sequentially. This may be null.
   * @param selectorAtBroker if true, selectors will be evaluated on the broker side. This may be null.
   * @return the Publisher instance.
   * @exception MOMException if the system fails to instantiate the Publisher due to some internal error.
   */
  public static Publisher publisher(String username, String password, String brokerList, Boolean loadBalancing, Boolean sequential, Boolean selectorAtBroker) throws MOMException {
    return new DefaultPublisherImpl(username, password, brokerList, loadBalancing, sequential, selectorAtBroker);
  }

  /**
   * Create a new Subscriber instance using default properties.
   *
   * @return the Subscriber instance.
   * @exception MOMException if the system fails to instantiate a Subscriber due to some internal error.
   */
  public static Subscriber subscriber() throws MOMException {
    return new DefaultSubscriberImpl();
  }

  /**
   * Create a new Subscriber instance.
   *
   * @param username the user name. This may be null.
   * @param password the password. This may be null.
   * @param brokerList the coma separated broker URLs list (in the form [protocol://]hostname[:port]). This may be null.
   * @param loadBalancing if true, indicates that the client is willing to have a connect request re-directed to another broker within a cluster. This may be null.
   * @param sequential if true, the broker list will be scanned sequentially. This may be null.
   * @param selectorAtBroker if true, selectors will be evaluated on the broker side. This may be null.
   * @return the Subscriber instance.
   * @exception MOMException if the system fails to instantiate a Subscriber due to some internal error.
   */
  public static Subscriber subscriber(String username, String password, String brokerList, Boolean loadBalancing, Boolean sequential, Boolean selectorAtBroker) throws MOMException {
    return new DefaultSubscriberImpl(username, password, brokerList, loadBalancing, sequential, selectorAtBroker);
  }
}

package cern.laser.business.pojo;

import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Properties;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.TextMessage;
import javax.jms.ObjectMessage;
import javax.jms.Session;
import javax.jms.Topic;
import javax.jms.TopicConnection;
import javax.jms.TopicConnectionFactory;
import javax.jms.TopicPublisher;
import javax.jms.TopicSession;
import javax.naming.Context;
import javax.naming.NamingException;

import org.apache.log4j.Logger;

import alma.alarmsystem.alarmmessage.AlarmMessageConversion;

import cern.laser.business.LaserCreateException;
import cern.laser.business.LaserRuntimeException;
import cern.laser.business.data.Alarm;
import cern.laser.business.data.AlarmChange;
import cern.laser.business.data.AlarmImpl;
import cern.laser.business.data.Category;
import cern.laser.business.data.Status;

import java.io.StringWriter;
import org.exolab.castor.xml.Marshaller;

public class AlarmPublisherImpl {
  private static final String LASER_INIT_PROPERTY = "LASER_INIT";
  private static final String LASER_SEARCH_PROPERTY = "LASER_SEARCH";
  private static final Logger LOGGER = Logger.getLogger(AlarmPublisherImpl.class.getName());

  private static final String DEFAULT_CATEGORY_ROOT_TOPIC = "CMW.ALARM_SYSTEM.CATEGORIES";

  private static final String BOOLEAN_SUFFIX = "_BOOL";
  private static final String INTEGER_SUFFIX = "_INT";
  private static final String FLOAT_SUFFIX = "_FLOAT";
  private static final String DOUBLE_SUFFIX = "_DOUBLE";
  private static final String SHORT_SUFFIX = "_SHORT";
  private static final String LONG_SUFFIX = "_LONG";
  private static final String STRING_SUFFIX = "_STRING";
  private static final String BYTE_SUFFIX = "_BYTE";

  private Context context = null;
  private TopicConnection connection = null;
  private TopicSession session = null;
  private TopicPublisher publisher = null;

  private TopicConnectionFactory topicConnectionFactory;
  private String categoryRootTopic = DEFAULT_CATEGORY_ROOT_TOPIC;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  public AlarmPublisherImpl() {
    try {
      //      getTopicConnection();
      //      getTopicSession();
    } catch (Exception e) {
      throw new LaserCreateException("unable to instantiate an alarm publisher", e);
    }
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  public void setTopicConnectionFactory(TopicConnectionFactory topicConnectionFactory) {
    this.topicConnectionFactory = topicConnectionFactory;
  }

  public void setCategoryRootTopic(String categoryRootTopic) {
    this.categoryRootTopic = categoryRootTopic;
  }

  public void publish(AlarmChange alarmChange) {
    if (alarmChange == null) {
      LOGGER.error("alarm change is null");
      return;
    }
    try {
      LOGGER.info("publishing alarm change for " + alarmChange.getCurrent().getAlarmId() + ", active= "
          + alarmChange.getCurrent().getStatus().getActive());
      Alarm alarm = alarmChange.getCurrent();
      Alarm previous = alarmChange.getPrevious();
      Iterator iterator = alarm.getCategories().iterator();
      while (iterator.hasNext()) {
        Category category = (Category) iterator.next();
        String destination = categoryRootTopic + "." + category.getPath();

        Topic topic = getTopicSession().createTopic(destination);
        //ObjectMessage message = getTopicSession().createObjectMessage((AlarmImpl) alarm);
        TextMessage message = getTopicSession().createTextMessage();
        setMessageProperties(message, alarm);
        
        Status previous_alarm_status = previous.getStatus();
        Status current_alarm_status = alarm.getStatus();

        message.setObjectProperty("REDUCED_MASKED_SET", Boolean.FALSE);
        /**
         * change belongs to the reduced set if and only if the transition is from whatever to (ACTIVE, NOT REDUCED, NOT
         * MASKED) or from (ACTIVE, NOT REDUCED, NOT MASKED) to whatever else or if there was not a transition but a
         * change of something NOT REDUCED and NOT MASKED
         */
        if (current_alarm_status.getActive().booleanValue()
            && !(current_alarm_status.getMasked().booleanValue() || current_alarm_status.getReduced().booleanValue())) {
          // transition to (ACTIVE, NOT REDUCED, NOT MASKED) or change of
          // something NOT REDUCED and NOT MASKED
          message.setObjectProperty("REDUCED_MASKED_SET", Boolean.TRUE);
        } else {
          // transition from (ACTIVE, NOT REDUCED, NOT MASKED) to something
          // else
          if (previous_alarm_status.getActive().booleanValue()
              && !(previous_alarm_status.getMasked().booleanValue() || previous_alarm_status.getReduced()
                  .booleanValue())) {
            message.setObjectProperty("REDUCED_MASKED_SET", Boolean.TRUE);
          }
        }

        message.setObjectProperty("NOT_REDUCED_MASKED_SET", Boolean.FALSE);
        /**
         * change belongs to the not reduced set if an only if the transition was from ACTIVE to NOT ACTIVE or from NOT
         * ACTIVE to ACTIVE or if it was not triggered by reduction or mask flags
         */
        if (current_alarm_status.getActive().booleanValue() != previous_alarm_status.getActive().booleanValue()) {
          // transition from ACTIVE to NOT ACTIVE or from NOT ACTIVE to ACTIVE
          message.setObjectProperty("NOT_REDUCED_MASKED_SET", Boolean.TRUE);
        } else if (current_alarm_status.getActive().booleanValue()
            && (current_alarm_status.getMasked().booleanValue() == previous_alarm_status.getMasked().booleanValue())
            && (current_alarm_status.getReduced().booleanValue() == previous_alarm_status.getReduced().booleanValue())) {
          // change not triggered by reduction or mask flags
          message.setObjectProperty("NOT_REDUCED_MASKED_SET", Boolean.TRUE);
        }

        // I insert the xml representation of this object
        // in the text field of the message
        String xml = AlarmMessageConversion.getXML((AlarmImpl)alarm);
        message.setText(xml);
        
        getTopicPublisher().publish(topic, message);
        LOGGER.info("change published on : " + destination);
      }
    } catch (Exception e) {
      LOGGER.error("unable to publish", e);
      System.err.println("*** Exception: Unable to publish "+e.getMessage());
      e.printStackTrace();
      close();
    }
  }

  public void sendInit(AlarmImpl alarm, String destination) {
    try {
      Topic topic = getTopicSession().createTopic(destination);
      TextMessage message = getTopicSession().createTextMessage();
      message.setText(AlarmMessageConversion.getXML(alarm));
      setMessageProperties(message, alarm);

      message.setObjectProperty("NOT_REDUCED_MASKED_SET", Boolean.TRUE);
      if (!(alarm.getStatus().getMasked().booleanValue() || alarm.getStatus().getReduced().booleanValue())) {
        message.setObjectProperty("REDUCED_MASKED_SET", Boolean.TRUE);
      } else {
        message.setObjectProperty("REDUCED_MASKED_SET", Boolean.FALSE);
      }

      getTopicPublisher().publish(topic, message);
    } catch (Exception e) {
      LOGGER.error("unable to send alarm " + alarm.getAlarmId() + " to destination " + destination, e);
      close();
      //throw new EJBException("unable to send alarm " + alarm.getAlarmId() + " to destination " + destination, e);
    }
  }

  public void sendInit(Collection alarms, String destination) {
    try {
      LOGGER.info("sending " + alarms.size() + " initial alarm(s) to " + destination + "...");

      Topic topic = getTopicSession().createTopic(destination);
      TextMessage message = getTopicSession().createTextMessage();
      Iterator iterator = alarms.iterator();
      while (iterator.hasNext()) {
        AlarmImpl alarm = (AlarmImpl) iterator.next();
        message.setText(AlarmMessageConversion.getXML(alarm));
        message.clearProperties();
        setMessageProperties(message, alarm);

        message.setObjectProperty("NOT_REDUCED_MASKED_SET", Boolean.TRUE);
        if (!(alarm.getStatus().getMasked().booleanValue() || alarm.getStatus().getReduced().booleanValue())) {
          message.setObjectProperty("REDUCED_MASKED_SET", Boolean.TRUE);
        } else {
          message.setObjectProperty("REDUCED_MASKED_SET", Boolean.FALSE);
        }
        getTopicPublisher().publish(topic, message);
      }
      LOGGER.info("initial alarm(s) sent to " + destination);
    } catch (Exception e) {
      LOGGER.error("unable to send initial alarms to " + destination+" : "+e.getMessage());
      close();
      throw new LaserRuntimeException("unable to send alarms to " + destination+" : "+e.getMessage());
    }
  }

  public void publish(Collection alarmChanges) {
    Iterator iterator = alarmChanges.iterator();
    while (iterator.hasNext()) {
      publish((AlarmChange) iterator.next());
    }
  }

  public String getCategoryRootTopic() {
    return categoryRootTopic;
  }

  public void sendInitFinished(String destination) {
    try {
      LOGGER.info("sending init finished message to " + destination + "...");
      Topic topic = getTopicSession().createTopic(destination);
      Message message = getTopicSession().createMessage();
      message.setObjectProperty(LASER_INIT_PROPERTY, Boolean.TRUE);
      getTopicPublisher().publish(topic, message);
      LOGGER.info("init finished message sent to " + destination);
    } catch (Exception e) {
      //      LOGGER.error("unable to send init finished message to "+destination,
      // e);
      close();
      //throw new EJBException("unable to send init finished message to " + destination, e);
    }
  }

  /**
   * @param init_alarms
   * @param destination
   */
  public void sendSearch(Collection alarms, String destination) {
    try {
      LOGGER.info("sending " + alarms.size() + " search alarm(s) to " + destination + "...");

      Topic topic = getTopicSession().createTopic(destination);
      ObjectMessage message = getTopicSession().createObjectMessage();
      Iterator iterator = alarms.iterator();
      while (iterator.hasNext()) {
        AlarmImpl alarm = (AlarmImpl) iterator.next();
        message.setObject(alarm);
        message.clearProperties();
        setMessageProperties(message, alarm);

        getTopicPublisher().publish(topic, message);
      }
      LOGGER.info("search alarm(s) sent to " + destination);
    } catch (Exception e) {
      LOGGER.error("unable to send search alarms to " + destination+" : "+ e.getMessage());
      close();
      throw new LaserRuntimeException("unable to send search alarms to " + destination+" : "+e.getMessage());
    }
    sendSearchFinished(destination);
  }

  //
  // -- PRIVATE METHODS --------------------------------------------
  //

  private void sendSearchFinished(String destination) {
    try {
      LOGGER.info("sending search finished message to " + destination + "...");
      Topic topic = getTopicSession().createTopic(destination);
      Message message = getTopicSession().createMessage();
      message.setObjectProperty(LASER_SEARCH_PROPERTY, Boolean.TRUE);
      getTopicPublisher().publish(topic, message);
      LOGGER.info("search finished message sent to " + destination);
    } catch (Exception e) {
      LOGGER.error("unable to send search finished message to " + destination+" : "+ e.getMessage());
      close();
      throw new LaserRuntimeException("unable to send search finished message to " + destination+" : "+ e.getMessage());
    }
  }

  //  private Context getInitialContext() throws NamingException {
  //    if (context == null) {
  //      context = new InitialContext();
  //    }
  //
  //    return context;
  //  }

  private TopicSession getTopicSession() throws JMSException, NamingException {
    if (session == null) {
      session = getTopicConnection().createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
    }

    return session;
  }

  private TopicConnection getTopicConnection() throws JMSException, NamingException {
    if (connection == null) {
      //      TopicConnectionFactory tcf = (TopicConnectionFactory)
      // getInitialContext().lookup("SonicJMS/TopicConnectionFactory");
      connection = topicConnectionFactory.createTopicConnection();
    }

    return connection;
  }

  private TopicPublisher getTopicPublisher() throws JMSException, NamingException {
    if (publisher == null) {
      publisher = getTopicSession().createPublisher(null);
    }

    return publisher;
  }

  private void close() {
    if (session != null) {
      try {
        session.close();
      } catch (JMSException je) {
        LOGGER.error("unable to close JMS session", je);
      }
    }
    if (connection != null) {
      try {
        connection.close();
      } catch (JMSException je) {
        LOGGER.error("unable to close JMS connection", je);
      }
    }
    publisher = null;
    session = null;
    connection = null;
  }

  private void setMessageProperties(Message message, Alarm alarm) throws JMSException {
	  if (message==null || alarm==null) {
		  throw new NullPointerException("setMessageProperties: message and/or alarm is null");
	  }
    // alarm id
    message.setObjectProperty("ALARM_ID", alarm.getAlarmId());
    // triplet
    message.setObjectProperty("FAULT_FAMILY", alarm.getTriplet().getFaultFamily());
    message.setObjectProperty("FAULT_MEMBER", alarm.getTriplet().getFaultMember());
    message.setObjectProperty("FAULT_CODE", alarm.getTriplet().getFaultCode());
    // problem description
    if (alarm.getProblemDescription() != null) {
      message.setObjectProperty("PROBLEM_DESCRIPTION", alarm.getProblemDescription());
    }
    // location
    if (alarm.getLocation() != null) {
      if (alarm.getLocation().getBuilding() != null) {
        if (alarm.getLocation().getBuilding().getBuildingNumber() != null) {
          message.setObjectProperty("BUILDING", alarm.getLocation().getBuilding().getBuildingNumber());
        }
        if (alarm.getLocation().getBuilding().getSite() != null) {
          message.setObjectProperty("SITE", alarm.getLocation().getBuilding().getSite());
        }
        if (alarm.getLocation().getBuilding().getZone() != null) {
          message.setObjectProperty("SAFETY_ZONE", alarm.getLocation().getBuilding().getZone());
        }
      }
    }
    // priority
    message.setObjectProperty("PRIORITY", alarm.getPriority());
    // source
    message.setObjectProperty("SOURCE_NAME", alarm.getSource().getName());
    // responsible person
    if (alarm.getResponsiblePerson() != null) {
      message.setObjectProperty("RESPONSIBLE_PERSON", alarm.getResponsiblePerson().getFirstName() + " "
          + alarm.getResponsiblePerson().getFamilyName());
    }
    // system name
    message.setObjectProperty("SYSTEM_NAME", alarm.getSystemName());
    // identifier
    message.setObjectProperty("IDENTIFIER", alarm.getIdentifier());
    // status
    message.setObjectProperty("ACTIVE", alarm.getStatus().getActive());
    message.setObjectProperty("REDUCED", alarm.getStatus().getReduced());
    message.setObjectProperty("MASKED", alarm.getStatus().getMasked());
    // user properties
    Properties user_properties = alarm.getStatus().getProperties();
    if ((user_properties != null) && (user_properties.size() != 0)) {
      Enumeration keys = user_properties.propertyNames();
      while (keys.hasMoreElements()) {
        String key = (String) keys.nextElement();
        
        String value = user_properties.getProperty(key);
        if (key.toUpperCase().endsWith(BOOLEAN_SUFFIX)) {
          message.setObjectProperty(key, Boolean.valueOf(value));
        } else if (key.toUpperCase().endsWith(INTEGER_SUFFIX)) {
          try {
            message.setObjectProperty(key, Integer.valueOf(value));
          } catch (NumberFormatException nfe) {
            LOGGER.warn("integer property was expected, found : " + value);
          }
        } else if (key.toUpperCase().endsWith(DOUBLE_SUFFIX)) {
          try {
            message.setObjectProperty(key, Double.valueOf(value));
          } catch (NumberFormatException nfe) {
            LOGGER.warn("double property was expected, found : " + value);
          }
        } else if (key.toUpperCase().endsWith(FLOAT_SUFFIX)) {
          try {
            message.setObjectProperty(key, Float.valueOf(value));
          } catch (NumberFormatException nfe) {
            LOGGER.warn("float property was expected, found : " + value);
          }
        } else if (key.toUpperCase().endsWith(SHORT_SUFFIX)) {
          try {
            message.setObjectProperty(key, Short.valueOf(value));
          } catch (NumberFormatException nfe) {
            LOGGER.warn("short property was expected, found : " + value);
          }
        } else if (key.toUpperCase().endsWith(BYTE_SUFFIX)) {
          try {
            message.setObjectProperty(key, Byte.valueOf(value));
          } catch (NumberFormatException nfe) {
            LOGGER.warn("byte property was expected, found : " + value);
          }
        } else {
          message.setStringProperty(key, value);
        }
      }
    }
  }
}

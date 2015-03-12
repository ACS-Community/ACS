/*
 * AlarmSystemInterfaceProxy.java
 *
 * Created on June 5, 2001, 10:21 AM
 */
package cern.laser.source.alarmsysteminterface.impl;

import java.net.UnknownHostException;
import java.util.Collection;
import java.util.Iterator;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.jms.TextMessage;

import org.apache.log4j.Category;

import cern.cmw.mom.pubsub.ExceptionListener;
import cern.cmw.mom.pubsub.MOMException;
import cern.cmw.mom.pubsub.PubSubFactory;
import cern.cmw.mom.pubsub.Publisher;
import cern.laser.source.alarmsysteminterface.ASIException;
import cern.laser.source.alarmsysteminterface.AlarmSystemInterface;
import cern.laser.source.alarmsysteminterface.FaultState;
import cern.laser.source.alarmsysteminterface.impl.configuration.ASIConfiguration;
import cern.laser.source.alarmsysteminterface.impl.message.ASIMessage;
import cern.laser.util.buffer.PullEvent;
import cern.laser.util.buffer.PullException;
import cern.laser.util.buffer.SynchroBuffer;
import cern.laser.util.buffer.SynchroBufferListener;

import alma.acs.util.IsoDateFormat;

/**
 * This class is the implementation of the interface between an alarm source
 * and the alarm system.
 * It provides the functionality defined in the interface AlarmSystemInterface.
 *
 * @author fracalde
 * @version 1.0
 */
public class AlarmSystemInterfaceProxy implements AlarmSystemInterface, SynchroBufferListener, ExceptionListener {
  /** logging category
   */
  private static Category cat = Category.getInstance(AlarmSystemInterfaceProxy.class.getName());

  /** configuration
   */
  private ASIConfiguration configuration;

  /** the publisher
   */
  private Publisher publisher;

  /** the hostname
   */
  private String hostName;

  /** the source name
   */
  private String sourceName = "UNDEFINED";

  /** the local buffer for packing alarm messages
   */
  private SynchroBuffer buffer;

  /** MOM connection flag
   */
  private AtomicBoolean connected = new AtomicBoolean(false);
  
  /** Create a new instance of AlarmSystemInterfaceProxy
   * @param sourceName the name of the alarm source
   */
  public AlarmSystemInterfaceProxy(String sourceName) throws ASIException {
    cat.info("instantiating the alarm system interface");
    
    /*
     * We force all the sources to use the same channel
     * 
     * The sourceName passes as a parameter is the name of the component
     * and was used to create a channel where the service listen to
     * (BTW this was wrong as the channel name is defined in the CDB
     * and there is no relation between the name of a component and
     * the channel)
     * 
     * There could be a performance problem here that may force us to use
     * more channels
     */
    sourceName="ALARM_SYSTEM_SOURCES";

    Configurator configurator = new Configurator();
    configuration = configurator.getConfiguration();
    setSourceName(sourceName);

    try {
      try {
        hostName = java.net.InetAddress.getLocalHost().getHostName().toUpperCase();

        if (hostName.endsWith(".CERN.CH")) {
          hostName = hostName.substring(0, hostName.indexOf(".CERN.CH"));
        }
      } catch (UnknownHostException uhe) {
        cat.warn("unable to get the host name", uhe);
        hostName = "";
      }

      // create a MOM publisher instance
      publisher = PubSubFactory.publisher();
      connected.set(true);
      publisher.setExceptionListener(this);

      // create and start the buffer manager
      buffer = new SynchroBuffer();
      buffer.setSynchroBufferListener(this);
      buffer.enable();
    } catch (Exception e) {
      ASIException asi_e = new ASIException("unable to instantiate the AlarmSystemInterfaceProxy : " + e.getMessage());
      asi_e.setRootCause(e);
      throw (asi_e);
    }
  }

  /**
   * Set the source name.
   * @param newSourceName the source name.
   */
  public void setSourceName(String newSourceName) {
    synchronized (sourceName) {
      sourceName = newSourceName;
    }
  }

  /**
   * Get the source name.
   * @return the source name.
   */
  public String getSourceName() {
    String source_name;

    synchronized (sourceName) {
      source_name = sourceName;
    }

    return source_name;
  }

  /** Close the instance.
   */
  public void close() {
    cat.info("closing...");

    // closing open threads and resources
    if (buffer != null) {
      buffer.close();
      buffer = null;
    }

    if (publisher != null) {
      publisher.close();
      publisher = null;
    }

    connected.set(false);
    cat.info("closed");
  }

  /** The exception handler called in case of communication exception
   *
   * @param e the MOMException caught
   *
   */
  public void onException(MOMException e) {
    if (e.testException(MOMException.CONNECTION_LOST_EXCEPTION)) {
      connected.set(false);
    } else if (e.testException(MOMException.CONNECTION_RECOVERED_EXCEPTION)) {
      connected.set(true);
    }
  }

  /** Implement the SysnchroBufferListener interface.
   * @param event the alarm message collection pulled from the buffer
   * @throws PullException if the messages can not be published
   */
  public void pull(PullEvent event) throws PullException {
    try {
      publish(event.getPulled(), false);
    } catch (Exception e) {
      cat.error("unable to publish message : " + e.getMessage(), e);
      throw new PullException("unable to pull objects from the buffer");
    }
  }

  /** Push a fault state.
   * @param state the fault state to push
   */
  public void push(FaultState state) throws ASIException {
    if (!connected.get()) {
      throw (new ASIException("not connected"));
    }

    if (state == null) {
      throw new IllegalArgumentException("state is null");
    }

    if (state instanceof FaultStateImpl) {
      ((FaultStateImpl) state).validate();
    }

    buffer.push(state);
  }

  /** Push an fault states collection.
   * @param states the fault states collection to push
   */
  public void push(Collection states) throws ASIException {
    if (!connected.get()) {
      throw (new ASIException("not connected"));
    }

    if (states == null) {
      throw new IllegalArgumentException("states collection is null");
    }

    Iterator iterator = states.iterator();

    while (iterator.hasNext()) {
      Object next = iterator.next();

      if (next instanceof FaultState) {
        push((FaultState) next);
      } else {
        throw new IllegalArgumentException("states collection does not contain FaultState instances");
      }
    }
  }

  /** Push the source active list forcing a backup
   * @param active the source active list
   */
  public void pushActiveList(Collection active) throws ASIException {
    if (!connected.get()) {
      throw (new ASIException("not connected"));
    }

    if (active == null) {
      throw (new IllegalArgumentException("active list is null"));
    }

    Iterator iterator = active.iterator();

    while (iterator.hasNext()) {
      Object next = iterator.next();

      if (next instanceof FaultStateImpl) {
        ((FaultStateImpl) next).validate();
      }
    }

    try {
      publish(active, true);
    } catch (Exception e) {
      cat.error("unable to publish backup : " + e.getMessage(), e);
    }
  }

  /** Publish a collection of fault states.
   * @param states the fault states collection to publish
   * @param backup the type of fault states to publish (backup or not)
   * @throw Exception if publishing fails
   */
  private void publish(Collection states, boolean backup) throws Exception {
    cat.debug("publishing " + states.size() + " fault state(s)");
    cat.debug("content :\n" + states.toString());

    ASIMessage asi_message = ASIMessageHelper.marshal(states);
    asi_message.setSourceName(getSourceName());
    asi_message.setSourceHostname(hostName);
   	asi_message.setSourceTimestamp(IsoDateFormat.formatCurrentDate());
    asi_message.setBackup(backup);
    asi_message.setVersion(configuration.getASIVersion());

    TextMessage alarm_message = publisher.createTextMessage();
    alarm_message.setText(XMLMessageHelper.marshal(asi_message));
    alarm_message.setStringProperty(configuration.getSourceNameProperty(), getSourceName());
    alarm_message.setStringProperty(configuration.getSourceHostnameProperty(), hostName);
    alarm_message.setStringProperty(configuration.getBackupProperty(), String.valueOf(backup));
    alarm_message.setStringProperty(configuration.getAlarmsNumberProperty(), String.valueOf(states.size()));

    StringBuffer topic = new StringBuffer(configuration.getAlarmsTopic());
    topic.append(".");
    topic.append(getSourceName());

    if (backup) {
      publisher.publish(topic.toString(), alarm_message, configuration.getBackupDeliveryMode(), configuration.getBackupPriority(), configuration.getBackupTimeToLive());
    } else {
      publisher.publish(topic.toString(), alarm_message, configuration.getChangesDeliveryMode(), configuration.getChangesPriority(), configuration.getChangesTimeToLive());
    }

    cat.debug("published");
  }
}

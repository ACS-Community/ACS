package cern.laser.business.pojo;

import java.sql.Timestamp;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import javax.jms.Message;
import javax.jms.TextMessage;

import org.apache.log4j.Logger;

import alma.alarmsystem.core.alarms.LaserCoreFaultState.LaserCoreFaultCodes;
import cern.laser.business.cache.AlarmCacheException;
import cern.laser.business.dao.SourceDAO;
import cern.laser.business.data.Alarm;
import cern.laser.business.data.Source;
import cern.laser.business.data.Status;
import cern.laser.business.data.Triplet;
import cern.laser.source.alarmsysteminterface.AlarmSystemInterfaceFactory;
import cern.laser.source.alarmsysteminterface.FaultState;
import cern.laser.source.alarmsysteminterface.impl.ASIMessageHelper;
import cern.laser.source.alarmsysteminterface.impl.XMLMessageHelper;
import cern.laser.source.alarmsysteminterface.impl.message.ASIMessage;
import cern.laser.util.LogTimeStamp;

import com.cosylab.acs.laser.LaserComponent;
import com.cosylab.acs.laser.dao.ACSAlarmCacheImpl;

import alma.acs.util.IsoDateFormat;

/**
 * CERN documentation is missing unfortunately... Below you can find ACS documentation of the changes
 * done on top of the CERN class.
 * <P>
 * The <code>AlarmMessageProcessorImpl</code> checks the timestamp of the alarms it receives from the NC
 * and publishes a core alarm ({@link LaserCoreFaultCodes#ALARMS_TOO_OLD}) 
 * if the delay between the actual time and the timestamp exceeds the threshold ({@link #delayThreashold}).
 * The check is done in this class because this is the place where alarms from sources are checked.
 * <BR>
 * To ensure that the alarm is set/cleared even if the alarms server does not process alarms, 
 * the check is done by the thread in {@link #createTimerTask()}.
 * <BR>
 * This feature is tested in the <code>alarmTest</code> module.
 * 
 * @author acaproni
 *
 */
public class AlarmMessageProcessorImpl {
  private static final Logger LOGGER = Logger.getLogger(AlarmMessageProcessorImpl.class.getName());

  private SourceDAO sourceDAO;
  private ACSAlarmCacheImpl alarmCache;
  
  /**
   * Helper class to record, for each time interval, the biggest delay between
   * the actual time and the timestamp of alarms.
   * 
   * @author acaproni
   * @since ACS 11.1
   *
   */
  private final class AlarmsDelayHelper {
	  /**
	   * Set to the bigger delay between the actual time and the message timestamp in
	   * the current time interval
	   */
	  private long largestDelay = 0;
	  
	  /**
	   * Check the passed timestamp against the actual time and 
	   * update the delay ({@link #largestDelay}) accordingly
	   * 
	   * @param timestamp The timestamp of a alarm 
	   * @throws RuntimeException If the timestamp is ahead of the actual time
	   */
	  public synchronized void updateDelay(long timestamp) throws RuntimeException {
		  long delay=System.currentTimeMillis()-timestamp;
		  if (delay<0) {
			  // This should never happen if the timestamp has been correctly set by
			  // the API.
			  throw new RuntimeException("The timestamp is ahead of actual time");
		  }
		  if (delay> largestDelay) {
			  largestDelay=delay;
		  }
	  }
	  
	  /**
	   * Check if the alarms are processed too late i.e. if their oldest timestamp in the last
	   * time interval has been greater then the threshold.
	   * <BR>This method reset the delay to be ready to check the delay during a new time interval.
	   * 
	   * @return <code>true</code> if the delay has been greater then the threshold
	   */
	  public synchronized boolean chekDelayAndReset() {
		  boolean ret=largestDelay>AlarmMessageProcessorImpl.delayThreashold;
		  largestDelay=0;
		  return ret;
	  }
	  
  }
  
  /**
   * The helper to check the timestamp of alarms against the actual time
   */
  private final AlarmsDelayHelper alarmsDelayHelper = new AlarmsDelayHelper();
  
  /**
   * If the timestamp of alarms and the actual time differ of more then 
   * <code>delayThreashold</code> then a core alarm is issued.
   */
  public static final long delayThreashold=TimeUnit.MILLISECONDS.convert(5, TimeUnit.MINUTES); // 5 min
  
  /**
   * Do not publish twice the alarm unless its state changed.
   * <P>
   * Actually the alarm server itself does this check but we do not want
   * to process this alarm every interval unless its state changed
   * (to reduce the load on the server).
   * <P>
   * Note that there is no need to synchronize on this variable
   * Because it used only by the timer thread to issue or not
   * a new alarm.
   */
  private boolean alarmTooOldActive=false;
  
  
  
  /**
   * The number of alarms processed in the past {@link #alarmTimestampsCheckInterval};
   */
  private final AtomicInteger alarmsProcessed = new AtomicInteger(0);
  
  /**
   * The timestamp of alarms is checked every <code>alarmTimestamsCheckInterval</code>
   * to reduce the risk to fill the alarm system publishing this alarm too often. 
   */
  public static final long alarmTimestampsCheckInterval=TimeUnit.MILLISECONDS.convert(5, TimeUnit.MINUTES); // 5 min
  
  /**
   * The LaserComponent to publish core alarms
   */
  private final LaserComponent laserComponent;
  
  /**
   * The timer to check the delay and do periodic tasks.
   */
  private final Timer timer = new Timer("AlarmMessageProcessorTimer",true);
  
  /**
   * Constructor
   * 
   * @param component The {@link LaserComponent}
   */
  public AlarmMessageProcessorImpl(LaserComponent component) {
	  if (component==null) {
		  throw new IllegalArgumentException("The LaserComponent can't be null");
	  }
	  laserComponent=component;
  }
  
  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  public void setSourceDAO(SourceDAO sourceDAO) {
    this.sourceDAO = sourceDAO;
  }

  public void setAlarmCache(ACSAlarmCacheImpl alarmCache) {
    this.alarmCache = alarmCache;
  }

  public void notifyReductionRelatives(Alarm alarm) {
    try {
      if (LOGGER.isDebugEnabled()) LOGGER.debug("notifying reduction relatives for " + alarm.getTriplet() + "...");
      notifyNodeChildren(alarm);
      notifyMultiplicityChildren(alarm);
      notifyMultiplicityParents(alarm);
      if (LOGGER.isDebugEnabled()) LOGGER.debug("notified");
    } catch (Exception e) {
      LOGGER.error("unable to notify reduction relatives for " + alarm.getTriplet(), e);
      System.out.println("Exception: "+e.getMessage());
      e.printStackTrace();
    }
  }

  public void process(Message alarmMessage) throws Exception {
	if (alarmMessage instanceof TextMessage) {
		TextMessage text_message = (TextMessage) alarmMessage;
	}
	  
    LOGGER.info("*** processing message... ***");
    if (LOGGER.isDebugEnabled()) LogTimeStamp.logMsg("processing message...", true);
    if (alarmMessage instanceof TextMessage) {
      TextMessage text_message = (TextMessage) alarmMessage;
      String xml_message = text_message.getText();
      process(xml_message);
    } else {
      throw new Exception("TextMessage expected");
    }
    LOGGER.info("*** message processed ***");
    if (LOGGER.isDebugEnabled()) LogTimeStamp.logMsg("message processed");
  }
  
  /**
   * Process a message
   * 
   * @param xml the XML message to unmarshall into a <code>ASIMessage</code>
   */
  public void process(String xml) throws Exception {
      ASIMessage asi_message = XMLMessageHelper.unmarshal(xml);
      LOGGER.info("message from " + asi_message.getSourceName() + "@" + asi_message.getSourceHostname());
      if (asi_message.getBackup()) {
        if (LOGGER.isDebugEnabled()) LogTimeStamp.logMsg("processing backup...");
        processBackup(asi_message);
        if (LOGGER.isDebugEnabled()) LogTimeStamp.logMsg("backup processed");
      } else {
        if (LOGGER.isDebugEnabled()) LogTimeStamp.logMsg("processing change...");
        processChanges(asi_message);
        if (LOGGER.isDebugEnabled()) LogTimeStamp.logMsg("change processed");
      }
      // One alarm more has been processed!
      alarmsProcessed.incrementAndGet();
      // Check for the delay comparing the actual time with the timestamp
      // of the processed message
      try {
    	  long timestamp = IsoDateFormat.parseIsoTimestamp(asi_message.getSourceTimestamp()).getTime();
    	  alarmsDelayHelper.updateDelay(timestamp);
      } catch (ParseException pe) {
    	  LOGGER.error("Error parsing a ISO timestamp: "+asi_message.getSourceTimestamp(), pe);
      }
      
  }

  public void processChange(FaultState faultState, String sourceName, String sourceHostname, Timestamp sourceTimestamp)
      throws Exception {
	alarmCache.acquire();
	try {
	    LOGGER.info("processing fault state: " + faultState.getFamily()+":"+faultState.getMember()+":"+faultState.getCode()+", Descriptor="+faultState.getDescriptor()+"\n");
	    Timestamp system_timestamp = new Timestamp(System.currentTimeMillis());
	    Alarm alarm = alarmCache.getCopy(Triplet.toIdentifier(faultState.getFamily(), faultState.getMember(), new Integer(
	        faultState.getCode())));
	    
	    // process the change
	    String defined_source_name = alarm.getSource().getName();
	    if (!defined_source_name.equals(sourceName)) {
	      LOGGER.error("source name mismatch : received " + sourceName + ", should be " + defined_source_name
	          + ".\nFault State was discarded :\n" + faultState);
	    } else {
	      Status current_status = alarm.getStatus();
	      boolean ordered = true;
	      // check if fault state changes were received in the right order
	      if ((current_status.getUserTimestamp() != null) && (faultState.getUserTimestamp() != null)) {
	        if (faultState.getUserTimestamp().before(current_status.getUserTimestamp())) {
	          LOGGER.error("user timestamp not ordered : received " + faultState.getUserTimestamp() + ", was "
	              + current_status.getUserTimestamp() + ".\nFault State was discarded :\n" + faultState);
	          System.err.println("user timestamp not ordered : received " + faultState.getUserTimestamp() + ", was "
	              + current_status.getUserTimestamp() + ".\nFault State was discarded :\n" + faultState);
	          ordered = false;
	        }
	      } else {
	        if (sourceTimestamp.before(current_status.getSourceTimestamp())) {
	          LOGGER.error("source timestamp not ordered : received " + sourceTimestamp + ", was "
	              + current_status.getSourceTimestamp() + ".\nFault State was discarded :\n" + faultState);
	          System.err.println("source timestamp not ordered : received " + sourceTimestamp + ", was "
	                  + current_status.getSourceTimestamp() + ".\nFault State was discarded :\n" + faultState);
	          ordered = false;
	        }
	      }
	      if (ordered) {
	        //        StatusImpl new_status = null;
	        //        StatusImpl alarm_status = alarm.getStatus();
	        boolean notify_reduction_relatives = false;
	        boolean alarm_updated = false;
	        if (alarm.getInstant().booleanValue()) {
	          if (faultState.getDescriptor().equalsIgnoreCase(FaultState.INSTANT)) {
	            // process INSTANT fault state
	            updateStatus(faultState, current_status, Boolean.TRUE, sourceHostname, sourceTimestamp, system_timestamp);
	            alarm_updated = true;
	            //            new_status = new StatusImpl(Boolean.TRUE, current_status.getMasked(), current_status.getReduced(), new
	            // Boolean(
	            //                faultState.getActivatedByBackup()), new Boolean(faultState.getTerminatedByBackup()), sourceHostname,
	            //                sourceTimestamp, faultState.getUserTimestamp(), system_timestamp, faultState.getUserProperties());
	          } else {
	            LOGGER.error("invalid fault descriptor : received " + faultState.getDescriptor() + "  while expecting "
	                + FaultState.INSTANT + ".\nFault State was discarded :\n" + faultState);
	          }
	        } else {
	          if (faultState.getDescriptor().equalsIgnoreCase(FaultState.ACTIVE)) {
	            // process ACTIVE fault state
	            if (alarm.getStatus().getActive().equals(Boolean.FALSE)) {
	              updateStatus(faultState, current_status, Boolean.TRUE, sourceHostname, sourceTimestamp, system_timestamp);
	              //              new_status = new StatusImpl(Boolean.TRUE, current_status.getMasked(), current_status.getReduced(),
	              //                  new Boolean(faultState.getActivatedByBackup()), new Boolean(faultState.getTerminatedByBackup()),
	              //                  sourceHostname, sourceTimestamp, faultState.getUserTimestamp(), system_timestamp, faultState
	              //                      .getUserProperties());
	              notify_reduction_relatives = true;
	              alarm_updated = true;
	            } else {
	              LOGGER.error("alarm already active.\nFault State was discarded :\n" + faultState);
	              System.err.println("*** Alarm already active.\nFault State was discarded :\n" + faultState);
	            }
	          } else if (faultState.getDescriptor().equalsIgnoreCase(FaultState.TERMINATE)) {
	            // process TERMINATE fault state
	            if (alarm.getStatus().getActive().equals(Boolean.TRUE)) {
	              updateStatus(faultState, current_status, Boolean.FALSE, sourceHostname, sourceTimestamp, system_timestamp);
	              //              new_status = new StatusImpl(Boolean.FALSE, current_status.getMasked(), current_status.getReduced(),
	              //                  new Boolean(faultState.getActivatedByBackup()), new Boolean(faultState.getTerminatedByBackup()),
	              //                  sourceHostname, sourceTimestamp, faultState.getUserTimestamp(), system_timestamp, faultState
	              //                      .getUserProperties());
	              notify_reduction_relatives = true;
	              alarm_updated = true;
	            } else {
	              LOGGER.error("alarm already terminated.\nFault State was discarded :\n" + faultState);
	            }
	          } else if (faultState.getDescriptor().equalsIgnoreCase(FaultState.CHANGE)) {
	            // process CHANGE fault state
	            if (alarm.getStatus().getActive().equals(Boolean.FALSE)) {
	              LOGGER.error("changed alarm was terminated : " + alarm.getAlarmId());
	              notify_reduction_relatives = true;
	            } else {
	              updateStatus(faultState, current_status, Boolean.TRUE, sourceHostname, sourceTimestamp, system_timestamp);
	              //            new_status = new StatusImpl(Boolean.TRUE, current_status.getMasked(), current_status.getReduced(), new
	              // Boolean(
	              //                faultState.getActivatedByBackup()), new Boolean(faultState.getTerminatedByBackup()), sourceHostname,
	              //                sourceTimestamp, faultState.getUserTimestamp(), system_timestamp, faultState.getUserProperties());
	              alarm_updated = true;
	            }
	          } else {
	            LOGGER.error("invalid fault descriptor : received " + faultState.getDescriptor() + "  while expecting "
	                + FaultState.ACTIVE + "|" + FaultState.TERMINATE + "|" + FaultState.CHANGE
	                + ".\nFault State was discarded :\n" + faultState);
	          }
	        }
	        //        if (new_status != null) {
	        if (alarm_updated) {
	          //          alarm.setStatus(new_status);
	          if (LOGGER.isDebugEnabled()) LOGGER.debug("applying change...");
	          if (alarm.getSource()!=null && sourceHostname!=null) {
	        	  alarm.getSource().setHostName(sourceHostname.toLowerCase());
	          }
	          
	          alarmCache.put(alarm);
	          if (notify_reduction_relatives) {
	            notifyReductionRelatives(alarm);
	          }
	        }
	      }
	    }
	} finally {
		alarmCache.release();
		LOGGER.info("processed fault state:" + faultState.getFamily()+":"+faultState.getMember()+":"+faultState.getCode()+", Descriptor="+faultState.getDescriptor()+"\n");
	}
  }

  private void updateStatus(FaultState faultState, Status currentStatus, Boolean active, String sourceHostname,
      Timestamp sourceTimestamp, Timestamp system_timestamp) {
    currentStatus.setActive(active);
    currentStatus.setActivatedByBackup(new Boolean(faultState.getActivatedByBackup()));
    currentStatus.setTerminatedByBackup(new Boolean(faultState.getTerminatedByBackup()));
    currentStatus.setSourceHostname(sourceHostname);
    currentStatus.setSourceTimestamp(sourceTimestamp);
    currentStatus.setUserTimestamp(faultState.getUserTimestamp());
    currentStatus.setSystemTimestamp(system_timestamp);
    String alarmServerProp = currentStatus.getProperties().getProperty(ACSAlarmCacheImpl.alarmServerPropkey);
   	currentStatus.setProperties(faultState.getUserProperties());
   	if (alarmServerProp!=null) {
   		currentStatus.getProperties().put(ACSAlarmCacheImpl.alarmServerPropkey, alarmServerProp);
   	}
  }

  public void updateMultiplicityNode(Alarm alarm) {
    try {
      if (hasTooManyActiveMultiplicityChildren(alarm)) {
        if (alarm.getStatus().getActive().equals(Boolean.FALSE)) {
          // activate multiplicity parent
          if (LOGGER.isDebugEnabled()) LOGGER.debug("activating multiplicity parent " + alarm.getTriplet());
          System.out.println("*** activating multiplicity parent " + alarm.getTriplet());
          alarm.getStatus().setActive(Boolean.TRUE);
          Timestamp current_time = new Timestamp(System.currentTimeMillis());
          alarm.getStatus().setSourceTimestamp(current_time);
          alarm.getStatus().setUserTimestamp(current_time);
          alarm.getStatus().setSystemTimestamp(current_time);
          alarmCache.put(alarm);
          notifyReductionRelatives(alarm);
        }
      } else {
        if (alarm.getStatus().getActive().equals(Boolean.TRUE)) {
          // terminate multiplicity parent
          if (LOGGER.isDebugEnabled()) LOGGER.debug("terminating multiplicity parent " + alarm.getTriplet());
          System.out.println("*** terminating multiplicity parent " + alarm.getTriplet());
          alarm.getStatus().setActive(Boolean.FALSE);
          Timestamp current_time = new Timestamp(System.currentTimeMillis());
          alarm.getStatus().setSourceTimestamp(current_time);
          alarm.getStatus().setUserTimestamp(current_time);
          alarm.getStatus().setSystemTimestamp(current_time);
          alarmCache.put(alarm);
          notifyReductionRelatives(alarm);
        }
      }
    } catch (Exception e) {
      LOGGER.error("unable to update multiplicity node for " + alarm.getTriplet(), e);
      System.err.println("unable to update multiplicity node for "+alarm.getTriplet());
      System.err.println("Exception; "+e.getMessage());
      e.printStackTrace();
    }
  }

  public void updateReductionStatus(Alarm alarm) {
    try {
      if (LOGGER.isDebugEnabled()) LOGGER.debug("updating reduction status for alarm : " + alarm);
      if (alarm.getStatus().getReduced().equals(Boolean.TRUE)) {
        if ((!hasActiveNodeParents(alarm)) && (!hasActiveMultiplicityParents(alarm))) {
          alarm.getStatus().setReduced(Boolean.FALSE);
          alarmCache.put(alarm);
        }
      } else {
        if (hasActiveNodeParents(alarm) || hasActiveMultiplicityParents(alarm)) {
          alarm.getStatus().setReduced(Boolean.TRUE);
          alarmCache.put(alarm);
        }
      }
      if (LOGGER.isDebugEnabled()) LOGGER.debug("updated");
    } catch (Exception e) {
      LOGGER.error("unable to update reduction status for " + alarm.getTriplet(), e);
    }
  }

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

  private boolean isMultiplicityParent(Alarm alarm) {
    return alarm.hasMultiplicityChildren();
  }

  private boolean isNodeParent(Alarm alarm) {
    return alarm.hasNodeChildren();
  }

  private boolean hasActiveMultiplicityParents(Alarm alarm) throws Exception {
    String[] parents = alarm.getMultiplicityParents();
    for (int i = 0; i < parents.length; i++) {
      Alarm parent = alarmCache.getReference(parents[i]);
      if (parent.getStatus().getActive().booleanValue()) { return true; }
    }

    return false;
  }

  private boolean hasActiveNodeParents(Alarm alarm) throws Exception {
    String[] parents = alarm.getNodeParents();
    for (int i = 0; i < parents.length; i++) {
      Alarm parent = alarmCache.getReference(parents[i]);
      if (parent.getStatus().getActive().booleanValue()) { return true; }
    }

    return false;
  }

  private boolean hasTooManyActiveMultiplicityChildren(Alarm alarm) throws Exception {
    int active_children = 0;
    String[] children = alarm.getMultiplicityChildren();
    for (int i = 0; i < children.length; i++) {
      Alarm child = alarmCache.getReference(children[i]);
      if (child.getStatus().getActive().booleanValue()) {
        active_children++;
      }
    }

    return (active_children > alarm.getMultiplicityThreshold().intValue());
  }

  private void processBackup(ASIMessage asiMessage) throws Exception {
    String source_name = asiMessage.getSourceName();
    String source_hostname = asiMessage.getSourceHostname();
    String source_timestamp = asiMessage.getSourceTimestamp();
    LOGGER.info("processing backup from " + source_name + "@" + source_hostname + " [" + source_timestamp + "]");
    Source source = sourceDAO.findSource(source_name);
  
    if (LOGGER.isDebugEnabled()) LOGGER.debug("getting active alarms...");
    String[] source_alarm_ids = sourceDAO.getAlarms(source.getSourceId());
    Collection active_alarms = new ArrayList(source_alarm_ids.length);
    for (int i = 0; i < source_alarm_ids.length; i++) {
      Alarm alarm = alarmCache.getReference(source_alarm_ids[i]);
      if (alarm.getStatus().getActive().booleanValue() && (!alarm.getInstant().booleanValue())) {
        active_alarms.add(alarm);
      }
    }
  
    if (LOGGER.isDebugEnabled()) LOGGER.debug("checking backup...");
    // check if backup alarms match active alarms for the same source
    Set active_identifiers = new HashSet();
    Iterator active_alarms_iterator = active_alarms.iterator();
    while (active_alarms_iterator.hasNext()) {
      active_identifiers.add(((Alarm) active_alarms_iterator.next()).getAlarmId());
    }
    
    Collection backup_fault_states = ASIMessageHelper.unmarshal(asiMessage);
    LOGGER.info("processing " + backup_fault_states.size() + " backup alarms");
    Set backup_identifiers = new HashSet();
    Iterator backup_fault_states_iterator = backup_fault_states.iterator();
    while (backup_fault_states_iterator.hasNext()) {
      FaultState fault_state = (FaultState) backup_fault_states_iterator.next();
      backup_identifiers.add(Triplet.toIdentifier(fault_state.getFamily(), fault_state.getMember(), new Integer(
          fault_state.getCode())));
    }
    LOGGER.info(asiMessage.getSourceName() + " : " + active_identifiers.size() + " active alarms found, received "
        + backup_identifiers.size());
    if ((backup_identifiers.containsAll(active_identifiers)) && (active_identifiers.containsAll(backup_identifiers))) {
      LOGGER.info("backup matches active alarms");
    } else {
      LOGGER.warn("backup mismatch");
      if (LOGGER.isDebugEnabled()) LOGGER.debug("alarms found :\n" + active_identifiers);
      if (LOGGER.isDebugEnabled()) LOGGER.debug("alarms received :\n" + backup_identifiers);
  
      Iterator backup_identifiers_iterator = backup_identifiers.iterator();
      while (backup_identifiers_iterator.hasNext()) {
        if (active_identifiers.remove(backup_identifiers_iterator.next())) {
          backup_identifiers_iterator.remove();
        }
      }
      if (!backup_identifiers.isEmpty()) {
        LOGGER.warn(backup_identifiers.size() + " backup alarms are not active");
        if (LOGGER.isDebugEnabled()) LOGGER.debug("active by backup :\n" + backup_identifiers);
        backup_fault_states_iterator = backup_fault_states.iterator();
        while (backup_fault_states_iterator.hasNext()) {
          FaultState fault_state = (FaultState) backup_fault_states_iterator.next();
          if (backup_identifiers.contains(Triplet.toIdentifier(fault_state.getFamily(), fault_state.getMember(),
              new Integer(fault_state.getCode())))) {
            fault_state.setDescriptor(FaultState.ACTIVE);
            fault_state.setActivatedByBackup(true);
            try {
            	Timestamp timestamp = new Timestamp(IsoDateFormat.parseIsoTimestamp(source_timestamp).getTime());
            	processChange(fault_state, source_name, source_hostname, timestamp);
            } catch (Exception e) {
              LOGGER.error("unable to activate alarm by backup", e);
            }
          }
        }
      }
      if (!active_identifiers.isEmpty()) {
        LOGGER.warn(active_identifiers.size() + " active alarms are not in backup");
        if (LOGGER.isDebugEnabled()) LOGGER.debug("terminate by backup :\n" + active_identifiers);
        active_alarms_iterator = active_alarms.iterator();
        while (active_alarms_iterator.hasNext()) {
          Alarm alarm = (Alarm) active_alarms_iterator.next();
          if (active_identifiers.contains(alarm.getAlarmId())) {
            FaultState fault_state = AlarmSystemInterfaceFactory.createFaultState(alarm.getTriplet().getFaultFamily(),
                alarm.getTriplet().getFaultMember(), alarm.getTriplet().getFaultCode().intValue());
            fault_state.setUserTimestamp(new Timestamp(System.currentTimeMillis()));
            fault_state.setUserProperties(alarm.getStatus().getProperties());
            fault_state.setDescriptor(FaultState.TERMINATE);
            fault_state.setTerminatedByBackup(true);
            try {
            	Timestamp timestamp = new Timestamp(IsoDateFormat.parseIsoTimestamp(source_timestamp).getTime());
            	processChange(fault_state, source_name, source_hostname, timestamp);
            } catch (Exception e) {
            	LOGGER.error("unable to terminate alarm by backup", e);
            }
          }
        }
      }
    }
  
    source.getStatus().setLastContact(new Timestamp(System.currentTimeMillis()));
    sourceDAO.updateSource(source);
  
    LOGGER.info("backup processed");
  }

  private void processChanges(ASIMessage asiMessage) {
    String source_name = asiMessage.getSourceName();
    String source_hostname = asiMessage.getSourceHostname();
    String source_timestamp = asiMessage.getSourceTimestamp();
    LOGGER.info("processing changes from " + source_name + "@" + source_hostname + " [" + source_timestamp + "]");
    Collection change_fault_states = ASIMessageHelper.unmarshal(asiMessage);

    LOGGER.info("processing " + change_fault_states.size() + " changes");
    Iterator iterator = change_fault_states.iterator();
    while (iterator.hasNext()) {
      FaultState fault_state = (FaultState) iterator.next();
      try {
    	  Timestamp timestamp = new Timestamp(IsoDateFormat.parseIsoTimestamp(source_timestamp).getTime());
    	  processChange(fault_state, source_name, source_hostname, timestamp);
      } catch (Exception e) {
        LOGGER.error("exception caught processing fault state : \n" + fault_state, e);
      }
    }
    LOGGER.info("changes processed");
  }

  private void notifyNodeChildren(Alarm alarm) throws Exception {
    String[] children = alarm.getNodeChildren();
    for (int i = 0; i < children.length; i++) {
      Alarm child = alarmCache.getCopy(children[i]);
      if (LOGGER.isDebugEnabled()) LOGGER.debug("notifying node child " + child.getTriplet());
      if (hasActiveNodeParents(child)) {
        if (child.getStatus().getReduced().equals(Boolean.FALSE)) {
          LOGGER.info("reducing node child " + child.getTriplet());
          child.getStatus().setReduced(Boolean.TRUE);
          alarmCache.put(child);
        }
      } else {
        if (child.getStatus().getReduced().equals(Boolean.TRUE)) {
          LOGGER.info("unreducing node child " + child.getTriplet());
          child.getStatus().setReduced(Boolean.FALSE);
          alarmCache.put(child);
        }
      }
    }
  }

  private void notifyMultiplicityChildren(Alarm alarm) throws Exception {
    String[] children = alarm.getMultiplicityChildren();
    for (int i = 0; i < children.length; i++) {
      Alarm child = alarmCache.getCopy(children[i]);
      if (LOGGER.isDebugEnabled()) LOGGER.debug("notifying multiplicity child " + child.getTriplet());
      if (hasActiveMultiplicityParents(child)) {
        if (child.getStatus().getReduced().equals(Boolean.FALSE)) {
          LOGGER.info("reducing multiplicity child " + child.getTriplet());
          child.getStatus().setReduced(Boolean.TRUE);
          alarmCache.put(child);
        }
      } else {
        if (child.getStatus().getReduced().equals(Boolean.TRUE)) {
          LOGGER.info("unreducing multiplicity child " + child.getTriplet());
          child.getStatus().setReduced(Boolean.FALSE);
          alarmCache.put(child);
        }
      }
    }
  }

  private void notifyMultiplicityParents(Alarm alarm) throws Exception {
    String[] parents = alarm.getMultiplicityParents();
    for (int i = 0; i < parents.length; i++) {
      Alarm parent = alarmCache.getCopy(parents[i]);
      if (LOGGER.isDebugEnabled()) LOGGER.debug("notifying multiplicity parent " + parent.getTriplet());
      updateMultiplicityNode(parent);
    }
  }
  
  private void dumpAlarm(Alarm alarm) {
	  System.out.print  ("\t"+alarm.getAlarmId()+", active="+alarm.getStatus().getActive());
	  System.out.print  (", reduced="+alarm.getStatus().getReduced());
	  System.out.println(", masked="+alarm.getStatus().getMasked());
  }
  
  /** 
   * Print on the stdout the state of the reduction of the passed alarm.
   * 
   * @param alarm
   */
  private void dumpAlarmReductionStatus(Alarm alarm) throws AlarmCacheException {
	  System.out.print("Reduction state of "+alarm.getAlarmId()+", active="+alarm.getStatus().getActive());
	  System.out.print(", isReduced="+alarm.getStatus().getReduced());
	  System.out.println(", isMasked="+alarm.getStatus().getMasked());
	  System.out.println("NODE Parents:");
	  String[] nodeP=alarm.getNodeParents();
	  for (String id: nodeP) {
		  Alarm parent=alarmCache.getReference(id);
		  dumpAlarm(parent);
	  }
	  System.out.println("NODE Childs:");
	  String[] nodeC=alarm.getNodeChildren();
	  for (String id: nodeC) {
		  Alarm child=alarmCache.getReference(id);
		  dumpAlarm(child);
	  }
	  System.out.println("MULTIPLICITY Parents:");
	  String[] multiP=alarm.getMultiplicityParents();
	  for (String id: multiP) {
		  Alarm parent=alarmCache.getReference(id);
		  dumpAlarm(parent);
	  }
	  System.out.println("MULTIPLICITY Childs:");
	  String[] multiC=alarm.getMultiplicityChildren();
	  for (String id: multiC) {
		  Alarm child=alarmCache.getReference(id);
		  dumpAlarm(child);
	  }
  }
  
  private TimerTask createTimerTask() {
	  TimerTask task = new TimerTask() {
		@Override
		public void run() {
			// Logs the number of alarms processed in the past interval (and reset the counter)
			LOGGER.debug(""+alarmsProcessed.getAndSet(0)+" alarms processed in the past 5 mins");
			// Check if the delay alarm must be published
			boolean alarmsAreTooLate=alarmsDelayHelper.chekDelayAndReset();
			if (alarmsAreTooLate!=alarmTooOldActive) {
				// Process the core alarm only if its state changed
				laserComponent.sendCoreAlarmAsync(LaserCoreFaultCodes.ALARMS_TOO_OLD,alarmsAreTooLate);
				alarmTooOldActive=alarmsAreTooLate;
			}
		}
	};
	return task;
  }
  
  /**
   * Start the periodic task
   */
  public void start() {
	  timer.schedule(createTimerTask(), alarmTimestampsCheckInterval, alarmTimestampsCheckInterval);
  }
  
  /**
   * Stop the periodic task
   */
  public void stop() {
	  timer.cancel();
  }

}

package cern.laser.business.pojo;

import java.util.Collection;
import java.util.Iterator;

import org.apache.log4j.Logger;

import cern.laser.business.LaserRuntimeException;
import cern.laser.business.dao.AlarmDAO;
import cern.laser.business.data.Alarm;
import cern.laser.business.data.AlarmChange;
import cern.laser.business.data.Status;
import cern.laser.util.LogTimeStamp;

public class AlarmCacheServerImpl {
  private static final Logger LOGGER = Logger.getLogger(AlarmCacheServerImpl.class.getName());

  private AlarmDAO alarmDAO;
  private AlarmPublisherImpl alarmPublisher;
  private MailAndSmsServerImpl mailAndSmsServer;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  public void setAlarmDAO(AlarmDAO alarmDAO) {
    this.alarmDAO = alarmDAO;
  }

  public void setAlarmPublisher(AlarmPublisherImpl alarmPublisher) {
    this.alarmPublisher = alarmPublisher;
  }

  public void setMailAndSmsServer(MailAndSmsServerImpl mailAndSmsServer) {
    this.mailAndSmsServer = mailAndSmsServer;
  }

  public Alarm load(String alarmId) {
    return alarmDAO.findAlarm(alarmId);
  }

  public void store(Collection updated) {
    try {
      LOGGER.info("storing " + updated.size() + " alarm(s)...");
      if (LOGGER.isDebugEnabled()) LogTimeStamp.logMsg("storing " + updated.size() + " alarm(s)...", true);
      Iterator iterator = updated.iterator();
      while (iterator.hasNext()) {
        Alarm alarm = (Alarm) iterator.next();
        Status status = alarm.getStatus();
//        if (status.getStatusId() == null || status.getStatusId().equals("")) {
//          if (LOGGER.isDebugEnabled()) LOGGER.debug("saving status ...");
//          status.setStatusId(alarm.getAlarmId());
//          alarmDAO.saveStatus(status);
//        } else {
//          if (LOGGER.isDebugEnabled()) LOGGER.debug("updating status ...");
          alarmDAO.updateStatus(status);
//        }
      }
      LOGGER.info("stored");
      if (LOGGER.isDebugEnabled()) LogTimeStamp.logMsg("stored");
    } catch (Exception e) {
      LOGGER.error("unable to store alarm(s)", e);
      throw new LaserRuntimeException("unable to store alarm(s)", e);
    }
  }

  public void publish(Collection alarmChanges) {
	  System.out.println("*** AlarmCacheServerImpl.Publishing "+alarmChanges.size()+" alarms");
    try {
      LOGGER.info("publishing " + alarmChanges.size() + " alarm(s)...");
      if (LOGGER.isDebugEnabled()) LogTimeStamp.logMsg("publishing " + alarmChanges.size() + " alarm(s)...", true);
      alarmPublisher.publish(alarmChanges);
      LOGGER.info("published");
      if (LOGGER.isDebugEnabled()) LogTimeStamp.logMsg("published");
    } catch (Exception e) {
      LOGGER.error("unable to publish alarm(s)", e);
      throw new LaserRuntimeException("unable to publish alarm(s)", e);
    }
  }

  public void notify(Collection alarmChanges) {
    try {
      Iterator iterator = alarmChanges.iterator();
      while (iterator.hasNext()) {
        AlarmChange alarm_change = (AlarmChange) iterator.next();
        Alarm current_alarm = alarm_change.getCurrent();
        if (!current_alarm.getStatus().getActive().equals(alarm_change.getPrevious().getStatus().getActive())) {
          if (current_alarm.getPiquetGSM() != null) {
            sendSMS(current_alarm);
          }
          if (current_alarm.getPiquetEmail() != null) {
            sendEmail(current_alarm);
          }
        }
      }
    } catch (Exception e) {
      LOGGER.error("unable to notify alarm(s)", e);
      throw new LaserRuntimeException("unable to notify alarm(s)", e);
    }
  }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

  private void sendEmail(Alarm currentAlarm) {
    try {
      LOGGER.info("notifying piquet email : " + currentAlarm.getPiquetEmail());
      if (LOGGER.isDebugEnabled())
          LogTimeStamp.logMsg("notifying piquet email : " + currentAlarm.getPiquetEmail(), true);
      String subject="Alarm server notification: "+currentAlarm.getAlarmId();
      if (currentAlarm.getStatus().getActive()) {
    	  subject=subject+" ACTIVE";
      } else {
    	  subject=subject+" TERMINATE";
      }
      mailAndSmsServer.sendEmail(currentAlarm.getPiquetEmail(), subject, currentAlarm.toString());
      LOGGER.info("notified");
      if (LOGGER.isDebugEnabled()) LogTimeStamp.logMsg("notified piquet email");
    } catch (Exception e) {
      LOGGER.error("unable to notify " + currentAlarm.getPiquetEmail() + " : " + currentAlarm.toString(), e);
    }
  }

  private void sendSMS(Alarm currentAlarm) {
    StringBuffer gsm_message = new StringBuffer();
    gsm_message.append(currentAlarm.getStatus().getSourceTimestamp());
    gsm_message.append(" => (");
    gsm_message.append(currentAlarm.getSystemName());
    gsm_message.append(")(");
    gsm_message.append(currentAlarm.getIdentifier());
    gsm_message.append(")(");
    gsm_message.append(currentAlarm.getProblemDescription());
    gsm_message.append(") IS ");
    gsm_message.append(currentAlarm.getStatus().getActive().booleanValue() ? "ACTIVE" : "TERMINATE");
    String number = "16" + currentAlarm.getPiquetGSM();
    try {
      LOGGER.info("notifying piquet GSM : " + number);
      if (LOGGER.isDebugEnabled()) LogTimeStamp.logMsg("notifying piquet GSM : " + number, true);
      mailAndSmsServer.sendSMS(number, gsm_message.toString());
      LOGGER.info("notified");
      if (LOGGER.isDebugEnabled()) LogTimeStamp.logMsg("notified piquet GSM");
    } catch (Exception e) {
      LOGGER.error("unable to notify " + number + " : " + gsm_message.toString(), e);
    }
  }
}
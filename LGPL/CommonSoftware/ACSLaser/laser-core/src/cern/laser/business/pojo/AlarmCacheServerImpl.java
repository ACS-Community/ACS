package cern.laser.business.pojo;

import java.util.Collection;
import java.util.Iterator;
import java.util.logging.Logger;

import alma.acs.logging.AcsLogLevel;
import cern.laser.business.LaserRuntimeException;
import cern.laser.business.dao.AlarmDAO;
import cern.laser.business.data.Alarm;
import cern.laser.business.data.AlarmChange;
import cern.laser.business.data.Status;
import cern.laser.util.LogTimeStamp;

public class AlarmCacheServerImpl {

  private AlarmDAO alarmDAO;
  private AlarmPublisherImpl alarmPublisher;
  private MailAndSmsServerImpl mailAndSmsServer;
  
  /**
   * ACS logger
   */
  private final Logger logger;

  public AlarmCacheServerImpl(Logger logger) {
	  if (logger==null) {
			throw new IllegalArgumentException("The logger can't be null");
		}
		this.logger=logger;
  }

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
      logger.log(AcsLogLevel.DELOUSE, "storing " + updated.size() + " alarm(s)...");
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
      logger.log(AcsLogLevel.DELOUSE, "stored");
    } catch (Exception e) {
      throw new LaserRuntimeException("unable to store alarm(s)", e);
    }
  }

  public void publish(Collection alarmChanges) {
    try {
    	logger.log(AcsLogLevel.DEBUG,"publishing " + alarmChanges.size() + " alarm(s)...");
      alarmPublisher.publish(alarmChanges);
      logger.log(AcsLogLevel.DEBUG,"published");
    } catch (Exception e) {
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
    	if (currentAlarm.getPiquetEmail()==null  || currentAlarm.getPiquetEmail().isEmpty()) {
    		logger.log(AcsLogLevel.DELOUSE,"No email address in "+currentAlarm.getAlarmId());
    		return;
    	}
    	logger.log(AcsLogLevel.DEBUG,"notifying piquet email : " + currentAlarm.getPiquetEmail());
      String subject="Alarm server notification: "+currentAlarm.getAlarmId();
      if (currentAlarm.getStatus().getActive()) {
    	  subject=subject+" ACTIVE";
      } else {
    	  subject=subject+" TERMINATE";
      }
      mailAndSmsServer.sendEmail(currentAlarm.getPiquetEmail(), subject, currentAlarm.toString());
      logger.log(AcsLogLevel.DELOUSE,"notified");
    } catch (Throwable t) {
    	logger.log(AcsLogLevel.ERROR,"unable to notify email to " + currentAlarm.getPiquetEmail() + " : " + currentAlarm.toString(), t);
    }
  }

  private void sendSMS(Alarm currentAlarm) {
	  // At the present, sending of SMS is not used in ALMA
	  return;
//    StringBuffer gsm_message = new StringBuffer();
//    gsm_message.append(currentAlarm.getStatus().getSourceTimestamp());
//    gsm_message.append(" => (");
//    gsm_message.append(currentAlarm.getSystemName());
//    gsm_message.append(")(");
//    gsm_message.append(currentAlarm.getIdentifier());
//    gsm_message.append(")(");
//    gsm_message.append(currentAlarm.getProblemDescription());
//    gsm_message.append(") IS ");
//    gsm_message.append(currentAlarm.getStatus().getActive().booleanValue() ? "ACTIVE" : "TERMINATE");
//    String number = "16" + currentAlarm.getPiquetGSM();
//    try {
//    	logger.log(AcsLogLevel.DELOUSE,"notifying piquet GSM : " + number);
//      mailAndSmsServer.sendSMS(number, gsm_message.toString());
//      logger.log(AcsLogLevel.DELOUSE,"notified");
//    } catch (Exception e) {
//    	logger.log(AcsLogLevel.DELOUSE,"unable to notify " + number + " : " + gsm_message.toString(), e);
//    }
  }
}
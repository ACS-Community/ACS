package cern.laser.business.cache;

import java.util.Collection;

import org.apache.log4j.Logger;

import cern.laser.business.data.AlarmChange;
import cern.laser.business.pojo.AlarmCacheServerImpl;
import cern.laser.util.buffer.PullEvent;
import cern.laser.util.buffer.PullException;
import cern.laser.util.buffer.SynchroBuffer;
import cern.laser.util.buffer.SynchroBufferListener;


/**
 * Implementation class.
 *
 * @author fracalde
 */
public class AlarmCacheListenerImpl implements AlarmCacheListener {
  private static final Logger LOGGER = Logger.getLogger(AlarmCacheListenerImpl.class.getName());

  private static final long TBP_CHECK_MIN_DELAY = 1000; //1 sec
  private static final long TBP_CHECK_MAX_DELAY = 5000; //5 sec
  private static final int TBP_CHECK_DELAY_SLOPE = 100;
  private static final long TBS_CHECK_MIN_DELAY = 1000; //1 sec
  private static final long TBS_CHECK_MAX_DELAY = 10000; //10 sec
  private static final int TBS_CHECK_DELAY_SLOPE = 200;
  private static final long TBN_CHECK_MIN_DELAY = 5000; //5 sec
  private static final long TBN_CHECK_MAX_DELAY = 30000; //30 sec
  private static final int TBN_CHECK_DELAY_SLOPE = 200;

  private AlarmCacheServerImpl alarmCacheServer;

//  private AlarmCacheServerSessionEJBLocal server = null;
//  private AlarmPublisherSessionEJBLocal alarmPublisher;
//  private MailAndSmsServerSessionEJBLocal mailAndSmsServer;

  private SynchroBuffer toBePublished;
  private SynchroBuffer toBeStored;
  private SynchroBuffer toBeNotified;

  public AlarmCacheListenerImpl(AlarmCacheServerImpl alarmCacheServer) {
    if (LOGGER.isDebugEnabled()) LOGGER.debug("Initializing alarm cache listener");
    this.alarmCacheServer = alarmCacheServer;

    // create the buffers for the changes to be published, persisted and notified via gsm/emails
    toBePublished = new SynchroBuffer(TBP_CHECK_MIN_DELAY, TBP_CHECK_MAX_DELAY, TBP_CHECK_DELAY_SLOPE, SynchroBuffer.DUPLICATE_OK);
    toBePublished.setSynchroBufferListener(createToBePublishedListener());
    toBePublished.enable();

    toBeStored = new SynchroBuffer(TBS_CHECK_MIN_DELAY, TBS_CHECK_MAX_DELAY, TBS_CHECK_DELAY_SLOPE, SynchroBuffer.DUPLICATE_REPLACE);
    toBeStored.setSynchroBufferListener(createToBeStoredListener());
    toBeStored.enable();
    
    toBeNotified = new SynchroBuffer(TBN_CHECK_MIN_DELAY, TBN_CHECK_MAX_DELAY, TBN_CHECK_DELAY_SLOPE, SynchroBuffer.DUPLICATE_REPLACE);
    toBeNotified.setSynchroBufferListener(createToBeNotifiedListener());
    toBeNotified.enable();
  }

  public void onAlarmChange(AlarmChange change) {
	  String str= "*** Message listener processing the message current="+change.getAlarmId()+" state="+change.getCurrent().getStatus().getActive();
	  str+=", prev="+change.getPrevious().getAlarmId()+" state="+change.getPrevious().getStatus().getActive();
	System.out.println(str);
    // push the change into the buffers
    toBePublished.push(change);
    toBeNotified.push(change);
    toBeStored.push(change.getCurrent());
  }

  public void close() {
    toBeNotified.close();
    toBePublished.close();
    toBeStored.close();
//    server = null;
//    alarmPublisher = null;
//    mailAndSmsServer = null;
  }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  protected void finalize() throws Throwable {
    close();
    super.finalize();
  }

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

  private SynchroBufferListener createToBePublishedListener() {
    return new SynchroBufferListener() {
        // buffer callback
        public void pull(PullEvent event) throws PullException {
          Collection alarm_changes = event.getPulled();

          if ((alarm_changes != null) && (alarm_changes.size() != 0)) {
            try {
              alarmCacheServer.publish(alarm_changes); //getAlarmCacheServerSessionEJBLocal().publish(alarm_changes);
            } catch (Exception e) {
              LOGGER.error("unable to publish changes : " + e.getMessage(), e);
              throw new PullException("unable to publish changes : " + e.getMessage());
            }
          }
        }
      };
  }

  private SynchroBufferListener createToBeStoredListener() {
    return new SynchroBufferListener() {
        // buffer callback
        public void pull(PullEvent event) throws PullException {
          Collection alarms = event.getPulled();

          if ((alarms != null) && (alarms.size() != 0)) {
            try {
              alarmCacheServer.store(alarms); //getAlarmCacheServerSessionEJBLocal().store(alarms);
            } catch (Exception e) {
              LOGGER.error("unable to store changes : " + e.getMessage(), e);
              throw new PullException("unable to store changes : " + e.getMessage());
            }
          }
        }
      };
  }

  private SynchroBufferListener createToBeNotifiedListener() {
    return new SynchroBufferListener() {
        // buffer callback
        public void pull(PullEvent event) throws PullException {
          Collection alarm_changes = event.getPulled();

          if ((alarm_changes != null) && (alarm_changes.size() != 0)) {
            try {
              alarmCacheServer.notify(alarm_changes);//getAlarmCacheServerSessionEJBLocal().notify(alarm_changes);
            } catch (Exception e) {
              LOGGER.error("unable to notify changes : " + e.getMessage(), e);
              throw new PullException("unable to notify changes : " + e.getMessage());
            }
          }
        }
      };
  }

//  private void publish(Collection alarmChanges) {
//    try {
//      LOGGER.info("publishing " + alarmChanges.size() + " alarm(s)...");
//      if (LOGGER.isDebugEnabled())
//        LogTimeStamp.logMsg("publishing " + alarmChanges.size() + " alarm(s)...", true);
//      getAlarmPublisherSessionEJBLocal().publish(alarmChanges);
//      LOGGER.info("published");
//      if (LOGGER.isDebugEnabled()) LogTimeStamp.logMsg("published");
//    } catch (Exception e) {
//      LOGGER.error("unable to publish alarm(s)", e);
//      throw new EJBException("unable to publish alarm(s)", e);
//    }
//  }
//
//  private void store(Collection updated) {
//  try {
//    LOGGER.info("storing " + updated.size() + " alarm(s)...");
//    if (LOGGER.isDebugEnabled()) LogTimeStamp.logMsg("storing " + updated.size() + " alarm(s)...", true);
//
//    Session session = PersistenceManager.currentSession();
//    Transaction tx = session.beginTransaction();
//
//    Iterator iterator = updated.iterator();
//    while (iterator.hasNext()) {
//      AlarmImpl alarm = (AlarmImpl) iterator.next();   
//      session.update(alarm.getStatus());
//    }
//
//    tx.commit();
//    PersistenceManager.closeSession();
//    LOGGER.info("stored");
//    if (LOGGER.isDebugEnabled()) LogTimeStamp.logMsg("stored");
//  } catch (Exception e) {
//    LOGGER.error("unable to store alarm(s)", e);
//    throw new EJBException("unable to store alarm(s)", e);
//  }
//}
//
//  public void notifyPiquet(Collection alarmChanges) {
//    try {
//      Iterator iterator = alarmChanges.iterator();
//      while (iterator.hasNext()) {
//        AlarmChange alarm_change = (AlarmChange) iterator.next();
//        if (!alarm_change.getCurrent().getStatus().getActive().equals(alarm_change.getPrevious().getStatus().getActive())) {
//          AlarmImpl current_alarm = alarm_change.getCurrent();
//          if (current_alarm.getPiquetGSM() != null) {
//            StringBuffer gsm_message = new StringBuffer();
//            gsm_message.append(current_alarm.getStatus().getSourceTimestamp());
//            gsm_message.append(" => (");
//            gsm_message.append(current_alarm.getSystemName());
//            gsm_message.append(")(");
//            gsm_message.append(current_alarm.getIdentifier());
//            gsm_message.append(")(");
//            gsm_message.append(current_alarm.getProblemDescription());
//            gsm_message.append(") IS ");
//            gsm_message.append(current_alarm.getStatus().getActive().booleanValue() ? "ACTIVE" : "TERMINATE");
//            String number = "16" + current_alarm.getPiquetGSM();
//            try {
//              LOGGER.info("notifying piquet GSM : " + number);
//              if (LOGGER.isDebugEnabled())
//                LogTimeStamp.logMsg("notifying piquet GSM : " + number, true);
//              getMailAndSmsServerSessionEJBLocal().sendSMS(number, gsm_message.toString());
//              LOGGER.info("notified");
//              if (LOGGER.isDebugEnabled())
//                LogTimeStamp.logMsg("notified piquet GSM");
//            } catch (Exception e) {
//              LOGGER.error("unable to notify " + number + " : " + gsm_message.toString(), e);
//            }
//          }
//          if (current_alarm.getPiquetEmail() != null) {
//            try {
//              LOGGER.info("notifying piquet email : " + current_alarm.getPiquetEmail());
//              if (LOGGER.isDebugEnabled())
//                LogTimeStamp.logMsg("notifying piquet email : " + current_alarm.getPiquetEmail(), true);
//              getMailAndSmsServerSessionEJBLocal().sendEmail(current_alarm.getPiquetEmail(), "LASER NOTIFICATION",
//                  current_alarm.toString());
//              LOGGER.info("notified");
//              if (LOGGER.isDebugEnabled())
//                LogTimeStamp.logMsg("notified piquet email");
//            } catch (Exception e) {
//              LOGGER.error("unable to notify " + alarm_change.getCurrent().getPiquetEmail() + " : " + current_alarm.toString(), e);
//            }
//          }
//        }
//      }
//    } catch (Exception e) {
//      LOGGER.error("unable to notify alarm(s)", e);
//      throw new EJBException("unable to notify alarm(s)", e);
//    }
//  }
//
//  private AlarmPublisherSessionEJBLocalHome getAlarmPublisherSessionEJBLocalHome() throws NamingException {
//    final InitialContext context = new InitialContext();
//    return (AlarmPublisherSessionEJBLocalHome) context.lookup("java:comp/env/ejb/AlarmPublisherSessionEJBLocal");
//  }
//
//  private AlarmPublisherSessionEJBLocal getAlarmPublisherSessionEJBLocal() throws Exception {
//    if (alarmPublisher == null) {
//      alarmPublisher = getAlarmPublisherSessionEJBLocalHome().create();
//    }
//
//    return alarmPublisher;
//  }
//
//  private MailAndSmsServerSessionEJBLocalHome getMailAndSmsServerSessionEJBLocalHome() throws NamingException {
//    final InitialContext context = new InitialContext();
//    return (MailAndSmsServerSessionEJBLocalHome) context.lookup("java:comp/env/ejb/MailAndSmsServerSessionEJBLocal");
//  }
//
//  private MailAndSmsServerSessionEJBLocal getMailAndSmsServerSessionEJBLocal() throws Exception {
//    if (mailAndSmsServer == null) {
//      mailAndSmsServer = getMailAndSmsServerSessionEJBLocalHome().create();
//    }
//
//    return mailAndSmsServer;
//  }
//
//    private AlarmCacheServerSessionEJBLocal getAlarmCacheServerSessionEJBLocal() throws Exception {
//    if (server == null) {
//      server = getAlarmCacheServerSessionEJBLocalHome().create();
//    }
//
//    return (AlarmCacheServerSessionEJBLocal) server;
//  }
//
//  private AlarmCacheServerSessionEJBLocalHome getAlarmCacheServerSessionEJBLocalHome() throws NamingException {
//    final InitialContext context = new InitialContext();
//
//    return (AlarmCacheServerSessionEJBLocalHome) context.lookup("java:comp/env/ejb/AlarmCacheServerSessionEJBLocal");
//  }
}

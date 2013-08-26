package cern.laser.business.pojo;

import java.net.InetAddress;
import java.sql.Timestamp;
import java.util.Timer;
import java.util.TimerTask;

import javax.naming.Context;

import org.apache.log4j.Logger;

import cern.laser.business.LaserRuntimeException;
import cern.laser.business.cache.AlarmCache;
import cern.laser.business.dao.SourceDAO;
import cern.laser.business.data.Alarm;
import cern.laser.business.data.Source;
import cern.laser.source.alarmsysteminterface.AlarmSystemInterfaceFactory;
import cern.laser.source.alarmsysteminterface.FaultState;

public class AlarmSourceMonitorImpl {
  private static Logger LOGGER = Logger.getLogger(AlarmSourceMonitorImpl.class.getName());
  private Context context = null;
  private Timer timer = null;
  private long delay;
  private static final long DEFAULT_DELAY = 60000;
  
  private SourceDAO sourceDAO;
  private AlarmMessageProcessorImpl alarmMessageProcessor;

  private long sourceMonitorFrequency = DEFAULT_DELAY;
  private AlarmCache alarmCache;
  
  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  public void setSourceDAO(SourceDAO sourceDAO) {
    this.sourceDAO = sourceDAO;
  }

  public void setAlarmMessageProcessor(AlarmMessageProcessorImpl alarmMessageProcessor) {
    this.alarmMessageProcessor = alarmMessageProcessor;
  }

  public void setSourceMonitorFrequency(long sourceMonitorFrequency) {
    this.sourceMonitorFrequency = sourceMonitorFrequency;
  }
  
  public void setAlarmCache(AlarmCache alarmCache) {
    this.alarmCache = alarmCache;
  }
  
  public void check() {
    try {
      LOGGER.debug("checking sources...");
      Source[] sources = sourceDAO.findAllSources();
      Source laser_source = sourceDAO.findByLaserSource();
      for (int i = 0; i < sources.length; i++) {
        Source source = sources[i];
        if (!source.equals(laser_source)) {
          if (source.isEnabled().booleanValue()) {
            if (source.getStatus().getLastContact() == null) {
              source.getStatus().setLastContact(new Timestamp(System.currentTimeMillis()));
            }
            if (((System.currentTimeMillis() - source.getStatus().getLastContact().getTime()) > source.getConnectionTimeout()
                .longValue())) {
              if (source.isConnected().booleanValue()) {
                // generate surveillance alarm
                Alarm surveillance_alarm = alarmCache.getCopy(source.getSurveillanceAlarmId());
                FaultState surveillance_fs = AlarmSystemInterfaceFactory.createFaultState(surveillance_alarm
                    .getTriplet().getFaultFamily(), surveillance_alarm.getTriplet().getFaultMember(),
                    surveillance_alarm.getTriplet().getFaultCode().intValue());
                Timestamp timestamp = new Timestamp(System.currentTimeMillis());
                surveillance_fs.setDescriptor(FaultState.ACTIVE);
                surveillance_fs.setUserTimestamp(timestamp);
                LOGGER.warn("generating surveillance alarm :\n" + surveillance_fs);
                alarmMessageProcessor.processChange(surveillance_fs, surveillance_alarm.getSource().getName(), InetAddress
                    .getLocalHost().getHostName(), timestamp);
                // set not connected
                source.getStatus().setConnected(Boolean.FALSE);
              }
            } else {
              if (!source.isConnected().booleanValue()) {
                // terminate surveillance alarm
                Alarm surveillance_alarm = alarmCache.getCopy(source.getSurveillanceAlarmId());
                FaultState surveillance_fs = AlarmSystemInterfaceFactory.createFaultState(surveillance_alarm
                    .getTriplet().getFaultFamily(), surveillance_alarm.getTriplet().getFaultMember(),
                    surveillance_alarm.getTriplet().getFaultCode().intValue());
                Timestamp timestamp = new Timestamp(System.currentTimeMillis());
                surveillance_fs.setDescriptor(FaultState.TERMINATE);
                surveillance_fs.setUserTimestamp(timestamp);
                LOGGER.warn("generating surveillance alarm :\n" + surveillance_fs);
                alarmMessageProcessor.processChange(surveillance_fs, surveillance_alarm.getSource().getName(), InetAddress
                    .getLocalHost().getHostName(), timestamp);
                // set connected
                source.getStatus().setConnected(Boolean.TRUE);
              }
            }
          }
        }
      }
      LOGGER.debug("sources checked");
    } catch (Exception e) {
      throw new LaserRuntimeException("unable to check sources", e);
    }
  }

  public boolean isUpToDate() {
    try {
      Source[] sources = sourceDAO.findAllSources();
      Source laser_source = sourceDAO.findByLaserSource();
      for (int i = 0; i < sources.length; i++) {
        Source source = sources[i];
        if (!source.equals(laser_source)) {
          if (!source.getStatus().getConnected().booleanValue()) { return false; }
        }
      }

      return true;
    } catch (Exception e) {
      throw new LaserRuntimeException("unable to check if sources are up to date", e);
    }
  }

  public void start() {
    try {
      if (timer == null) {
        delay = DEFAULT_DELAY;
        LOGGER.info("AlarmImpl Source Monitor check delay : " + delay + " milliseconds");
        LOGGER.info("starting source monitoring...");
        timer = new Timer();
        timer.schedule(createTimerTask(), delay, delay);
        LOGGER.info("source monitoring started");
      }
    } catch (Exception e) {
      throw new LaserRuntimeException("unable to start the source monitoring", e);
    }
  }

  public void stop() {
    try {
      if (timer != null) {
        LOGGER.info("stopping source monitoring...");
        timer.cancel();
        timer = null;
        LOGGER.info("source monitoring stopped");
      }
    } catch (Exception e) {
      throw new LaserRuntimeException("unable to stop the source monitoring", e);
    }
  }

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

  private TimerTask createTimerTask() {
    return new TimerTask() {
      public void run() {
        try {
          check();
        } catch (Exception e) {
          LOGGER.error("check failed : " + e.getMessage(), e);
        }
      }
    };
  }
}
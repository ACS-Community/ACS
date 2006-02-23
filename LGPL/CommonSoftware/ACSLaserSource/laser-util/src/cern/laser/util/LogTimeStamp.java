/*
 * $Id: LogTimeStamp.java,v 1.1.1.1 2005/03/29 10:03:02 acaproni Exp $
 *
 * $Date: 2005/03/29 10:03:02 $ 
 * $Revision: 1.1.1.1 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.util;

import java.util.HashMap;
import org.apache.log4j.Logger;

/**
 * 
 * 
 * @version $Revision: 1.1.1.1 $ $Date: 2005/03/29 10:03:02 $
 * @author Katarina Sigerud
 */
public class LogTimeStamp {
  private static HashMap ht = new HashMap();

  private static final Logger LOGGER = Logger.getLogger(LogTimeStamp.class.getName());

  private static class ThreadExecInfo {
    long timestamp;
    int stepno;
  }

  public static void logMsg(String Msg) {
    logMsg(Msg, false);
  }

  /*
   * Passing true in the second parameter of this function resets the counter
   * for the current thread. Otherwise it keeps track of the last invocation and
   * prints the current counter value and the time difference between the two
   * invocations.
   */
  public static void logMsg(String Msg, boolean flag) {
    LogTimeStamp.ThreadExecInfo thr;
    long timestamp = System.currentTimeMillis();

    synchronized (ht) {
      thr = (LogTimeStamp.ThreadExecInfo) ht.get(Thread.currentThread().getName());
      if (thr == null) {
        thr = new LogTimeStamp.ThreadExecInfo();
        ht.put(Thread.currentThread().getName(), thr);
      }
    }

    if (flag == true) {
      thr.stepno = 0;
      LOGGER.debug(Thread.currentThread().getName() + ":" + thr.stepno + ":" + Msg);
    }

    if (thr.stepno != 0) {
      LOGGER.debug(Thread.currentThread().getName() + ":" + thr.stepno + ":" + Msg + ":" + (timestamp - thr.timestamp));
    }
    thr.stepno = thr.stepno + 1;
    thr.timestamp = timestamp;
  }

}
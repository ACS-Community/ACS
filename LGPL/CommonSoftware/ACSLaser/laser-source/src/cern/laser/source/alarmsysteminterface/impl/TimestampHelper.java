/*
 * TimestampHelper.java
 *
 * Created on February 28, 2003, 7:02 PM
 */
package cern.laser.source.alarmsysteminterface.impl;

import java.sql.Timestamp;


/**
 * Helper class for marshaling/unmarshaling to/from java.sqlTimestamp and XML generated timestamps.
 * @author  fracalde
 */
public class TimestampHelper {
  /** Creates a new instance of TimestampHelper */
  private TimestampHelper() {
  }

  /**
   * DOCUMENT ME!
   *
   * @param timestamp DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public static cern.laser.source.alarmsysteminterface.impl.message.Timestamp marshalSourceTimestamp(Timestamp timestamp) {
    long millis = timestamp.getTime();
    int nanos = timestamp.getNanos();
    long seconds = (millis / 1000);
    long micros = (nanos / 1000);
    cern.laser.source.alarmsysteminterface.impl.message.Timestamp generated = new cern.laser.source.alarmsysteminterface.impl.message.Timestamp();
    generated.setSeconds(seconds);
    generated.setMicroseconds(micros);

    return generated;
  }

  /**
   * DOCUMENT ME!
   *
   * @param timestamp DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public static cern.laser.source.alarmsysteminterface.impl.message.Timestamp marshalUserTimestamp(Timestamp timestamp) {
    long millis = timestamp.getTime();
    int nanos = timestamp.getNanos();
    long seconds = (millis / 1000);
    long micros = (nanos / 1000);
    cern.laser.source.alarmsysteminterface.impl.message.Timestamp generated = new cern.laser.source.alarmsysteminterface.impl.message.Timestamp();
    generated.setSeconds(seconds);
    generated.setMicroseconds(micros);

    return generated;
  }

  /**
   * DOCUMENT ME!
   *
   * @param generated DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public static Timestamp unmarshalSourceTimestamp(cern.laser.source.alarmsysteminterface.impl.message.Timestamp generated) {
    long millis = (generated.getSeconds() * 1000);
    int nanos = (int) (generated.getMicroseconds() * 1000);
    Timestamp timestamp = new Timestamp(millis);
    timestamp.setNanos(nanos);

    return timestamp;
  }

  /**
   * DOCUMENT ME!
   *
   * @param generated DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public static Timestamp unmarshalUserTimestamp(cern.laser.source.alarmsysteminterface.impl.message.Timestamp generated) {
    long millis = (generated.getSeconds() * 1000);
    int nanos = (int) (generated.getMicroseconds() * 1000);
    Timestamp timestamp = new Timestamp(millis);
    timestamp.setNanos(nanos);

    return timestamp;
  }
}

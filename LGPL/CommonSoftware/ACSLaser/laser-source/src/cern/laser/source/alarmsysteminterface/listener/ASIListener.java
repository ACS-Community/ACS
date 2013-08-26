/*
 * ASIListener.java
 *
 * Created on February 27, 2003, 11:44 AM
 */

package cern.laser.source.alarmsysteminterface.listener;

import java.sql.Timestamp;
import java.util.Collection;

/**
 * ASIMessage listener interface.
 * @author  fracalde
 */
public interface ASIListener {
    /**
     * Callback method. Called on ASIMessage reception.
     * @param source the alarm source pushing the fault states.
     * @param sourceHostname the alarm source host name.
     * @param sourceTimestamp the alarm source timestamp.
     * @param backup true iff the message is a backup.
     * @param states the actual fault states collection.
     */
    public void onMessage(String source, String sourceHostname, Timestamp sourceTimestamp, boolean backup, Collection states);
}

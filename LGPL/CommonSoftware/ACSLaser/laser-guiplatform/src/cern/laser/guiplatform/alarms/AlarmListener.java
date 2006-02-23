/*
 * AlarmListener.java
 *
 * Created on May 21, 2003, 3:10 PM
 */

package cern.laser.guiplatform.alarms;

import java.util.Collection;

/**
 * This interfaces should be implemented by all AlarmNodeManager's
 * This is observer
 *
 * @author  pawlowsk
 */
public interface AlarmListener {

    /**
     * add an alarm to the children list 
     * @param al The alarm bean to added
     */    
    void addAlarm(AlarmBean alarm);
   
    /**
     * add an alarm collection to the children list 
     * @param als The alarm bean collection to added
     */    
    void addAlarms(Collection als);

    /**
     * remove an alarm
     * @param al The alarm bean to be removed
     */    
    void removeAlarm(AlarmBean alarm);

    /**
     * remove an alarm useing alarmId
     */
    void removeAlarm(String alarmId);
    /**
     * remove an alarm collection
     * @param al The alarm bean collection to be removed
     */    
    void removeAlarms(Collection als);
    
    /** removes all the alarms of the current list
     */
    //void removeAllAlarms();
    
    /** @return the number of active alarms
     */
    //int getAlarmsCount();
 
}

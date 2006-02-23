/*
 * AlarmsNumberChangeListener.java
 *
 * Created on June 3, 2003, 5:08 PM
 */

package cern.laser.guiplatform.alarms;

/**
 * This interface is used by Alarm statistic panel (Observer)
 *
 * @author  pawlowsk
 */
public interface AlarmsNumberChangeListener {
  
    // WARNING:     DO NOT CHANGE THIS NUMBERS
    //  
    /** active alarm counter */ 
    public final int ACTIVE_ALARM_COUNTER = 0;
    
    /** masked alarm counter */ 
    public final int MASKED_ALARM_COUNTER = 1;
    
    /** inhigited alarm counter */ 
    public final int INHIBITED_ALARM_COUNTER = 2;
    
    /** highlighted and klaxon counter */ 
    public final int HIGHLIGHTED_AND_KLAXON_ALARM_COUNTER = 3;
    
    /** highlighted counter */ 
    public final int HIGHLIGHTED_ALARM_COUNTER = 4;
    
    /** instant alarm counter */ 
    public final int INSTANT_ALARM_COUNTER = 5;

    /** number of all list */
    public final int ALL_LIST_NUMBER = 6;
        
    //void increase(final int counterType);
    
    //void decrease(final int counterType);

    /** 
     * This method subtracts 1 from fromCounterType and adds 1 to toCounterType
     *
     * @param fromCounterType i. e. AlarmNumberChangeListener.ACTIVE_ALARM_COUNTER
     * @param toCounterType i. e. AlarmNumberChangeListener.ACTIVE_ALARM_COUNTER 
     */
    public void moveAlarmNumber(final int fromCounterType, final int toCounterType);

    /** This mehtod is used to update listener (i. e. when user wants to add 
     * this listener to AlarmContainer
     * 
     * @param counterType (i. e. AlarmNumberChangeListener.ACTIVE_ALARM_COUNTER)
     * @param alarmNo new alarm number
     */
    public void update(final int counterType, final long alarmNo);

    /**
     * This method add 1 to counterType
     * @param counterType (i. e. AlarmNumberChangeListener.ACTIVE_ALARM_COUNTER)
     */
    public void increaseCounter(final int counterType);

    /**
     * This method substracts 1 from counterType
     * @param counterType (i. e. AlarmNumberChangeListener.ACTIVE_ALARM_COUNTER)
     */
    public void decreaseCounter(final int counterType);
 
    
}

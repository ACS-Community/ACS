/*
 * AlarmListener.java
 *
 * Created on May 16, 2003, 1:06 PM
 */

package cern.laser.guiplatform.alarms;

//import cern.laser.guiplatform.alarms.DisplayableChangeEvent;
import java.util.Collection;

/**
 * This is JMS alarm listener. This listener (class which implements this) will be 
 * register in middle tier.
 *
 * @depracted should not be used, this class was replaced by
 *          cern.laser.client.data.services.AlamSelectionListener
 *
 * @author  pawlowsk
 */
public interface JMSAlarmListener {
    
    /** this method will be invoked by business layer when on alarm arrives
     * @param changeEvent event from business layer
     */
    //void onAlarm(DisplayableChangeEvent changeEvent);
    
    /** this method will be invoked by business layer when more than one alarm
     * arrives
     * @param changedEvents collection with new alarms
     */
    void onAlarm(Collection changedEvents);
    
    /** method invoked by business layer when exception occurs */
    void onException(/* arguments, but I do not know which yet */);
    
}

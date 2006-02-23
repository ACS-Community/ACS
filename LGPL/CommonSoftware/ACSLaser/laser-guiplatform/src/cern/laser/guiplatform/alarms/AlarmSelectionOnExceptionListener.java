/*
 * AlarmSelectionOnExceptionListener.java
 *
 * Created on October 17, 2003, 5:48 PM
 */

package cern.laser.guiplatform.alarms;

/**
 * Class which should be notyfied when LaserSelectionException occurs, should 
 * implement this interface. In this case: AlarmStatisticInfoPanel
 *
 * @author  Bartlomiej Pawlowski <Bartlomiej.Pawlowski@cern.ch>
 */
public interface AlarmSelectionOnExceptionListener {
  
    
    void onException(final java.lang.String exceptionCode);
    
}

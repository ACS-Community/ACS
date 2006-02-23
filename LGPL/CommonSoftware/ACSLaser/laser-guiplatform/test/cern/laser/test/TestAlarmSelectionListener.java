/*
 * TestAlarmSelectionListener.java
 *
 * Created on August 29, 2003, 2:35 PM
 */

package cern.laser.test;

import org.apache.log4j.Logger;

import cern.laser.guiplatform.util.LogFactory;

/**
 *
 * @author  Bartlomiej Pawlowski <Bartlomiej.Pawlowski@cern.ch>
 */
public class TestAlarmSelectionListener implements cern.laser.client.services.selection.AlarmSelectionListener {
    
    public static Logger logger = LogFactory.getLogger(TestAlarmSelectionListener.class.getName());
    
    /** Creates a new instance of TestAlarmSelectionListener */
    public TestAlarmSelectionListener() {
    }
    
    public void onAlarm(cern.laser.client.data.Alarm alarm) {
        logger.debug("alarm arived !!!");
    }
    
    public void onException(cern.laser.client.services.selection.LaserSelectionException e) {
        logger.debug("exception occured !!!");
    }
    
}

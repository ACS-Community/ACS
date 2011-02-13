/*
 * AlarmBrowsingHandlerFactory.java
 *
 * Created on October 31, 2003, 2:13 PM
 */

package cern.laser.guiplatform.alarms;

import alma.acs.container.ContainerServicesBase;
import cern.laser.client.LaserException;
import cern.laser.client.services.browsing.AlarmBrowsingHandler;
import cern.laser.guiplatform.util.Constants;

/**
 *
 * @author  Bartlomiej Pawlowski <Bartlomiej.Pawlowski@cern.ch>
 */
public class AlarmBrowsingHandlerFactory {
    
    /** Creates a new instance of AlarmBrowsingHandlerFactory */
    private AlarmBrowsingHandlerFactory() {
    }
    
    public static final AlarmBrowsingHandler getHandler(ContainerServicesBase contSvcs) throws LaserException {
        
        AlarmBrowsingHandler handler = null;
        if ( Constants.getDefaultWorkingMode() == Constants.TEST_WORKING_MODE )
            throw new RuntimeException("There is no AlarmBrowsingHandler for TEST_WORKING_MODE");
        else 
            handler = AlarmBrowsingHandler.get(contSvcs);
        return handler;
        
        
    }
    
}

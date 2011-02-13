/*
 * AlarmReductionHandlerFactory.java
 *
 * Created on November 16, 2003, 4:46 PM
 */

package cern.laser.guiplatform.alarms;

import alma.acs.container.ContainerServicesBase;
import cern.laser.client.LaserException;
import cern.laser.client.services.reduction.AlarmReductionHandler;
import cern.laser.guiplatform.util.Constants;

/**
 *
 * @author  Bartlomiej Pawlowski <Bartlomiej.Pawlowski@cern.ch>
 */
public class AlarmReductionHandlerFactory {
    
    /** Creates a new instance of AlarmReductionHandlerFactory */
    private AlarmReductionHandlerFactory() {
    }
    
    public static AlarmReductionHandler getHandler(ContainerServicesBase contSvcs) throws LaserException {
        AlarmReductionHandler handler = null;
        if ( Constants.getDefaultWorkingMode() == Constants.TEST_WORKING_MODE )
            throw new RuntimeException("AlarmReductionHandler not implements for TEST_WORKING_MODE"); 
        else 
            handler = AlarmReductionHandler.get(contSvcs);
        return handler;

    }
    
}

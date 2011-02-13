/*
 * AlarmSelectionHandlerFactory.java
 *
 * Created on June 20, 2003, 11:19 AM
 */

package cern.laser.guiplatform.alarms;

import alma.acs.container.ContainerServicesBase;
import cern.laser.client.LaserException;
import cern.laser.client.services.selection.AlarmSelectionHandler;
import cern.laser.guiplatform.util.Constants;
/**
 *
 * @author  pawlowsk
 */
public class AlarmSelectionHandlerFactory {
    
    /** Creates a new instance of AlarmSelectionHandlerFactory 
     * do not instantiate
     */
    private AlarmSelectionHandlerFactory() {
    }
    
    public static AlarmSelectionHandler getHandler(ContainerServicesBase contSvcs) throws LaserException {
       
        AlarmSelectionHandler handler = null;
        if ( Constants.getDefaultWorkingMode() == Constants.TEST_WORKING_MODE )
            throw new RuntimeException("AlarmSelectionHandler not implements for TEST_WORKING_MODE"); 
        else 
            handler = AlarmSelectionHandler.get(contSvcs);
        return handler;
    }
    
}

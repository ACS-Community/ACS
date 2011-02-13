/*
 * UserHandlerFactory.java
 *
 * Created on July 14, 2003, 3:42 PM
 */

package cern.laser.guiplatform.user;

import alma.acs.container.ContainerServicesBase;
import cern.laser.console.LaserConsoleException;
import cern.laser.console.UserHandler;
import cern.laser.guiplatform.util.Constants;

/**
 * 
 * @author  pawlowsk
 */
public class UserHandlerFactory {
    
    /** Creates a new instance of UserHandlerFactory */
    private UserHandlerFactory() {
    }
    
    public static UserHandler getHandler(ContainerServicesBase contSvcs) throws LaserConsoleException {
        UserHandler handler = null;

        if ( Constants.getDefaultWorkingMode() == Constants.TEST_WORKING_MODE ) {
            handler = cern.laser.guiplatform.user.helpers.UserHandler.get(); 
        } else {
            handler = cern.laser.console.UserHandler.get(contSvcs);
        }
        return handler;
    
    }
    
}

/*
 * UserHandlerFactory.java
 *
 * Created on July 14, 2003, 3:42 PM
 */

package cern.laser.guiplatform.user;

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
    
    public static UserHandler getHandler() throws LaserConsoleException {
        UserHandler handler = null;

        if ( Constants.getDefaultWorkingMode() == Constants.TEST_WORKING_MODE ) {
        	System.out.println("### Getting the cern.laser.guiplatform.user.helpers.UserHandler");
            handler = cern.laser.guiplatform.user.helpers.UserHandler.get(); 
        } else {
        	System.out.println("### Getting the cern.laser.console.UserHandler");
            handler = cern.laser.console.UserHandler.get();
        }
        return handler;
    
    }
    
}

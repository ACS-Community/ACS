/*
 * CategoryBrowsingHandlerFactory.java
 *
 * Created on July 5, 2003, 4:05 PM
 */

package cern.laser.guiplatform.category;

import alma.acs.container.ContainerServicesBase;
import cern.laser.client.LaserException;
import cern.laser.client.services.browsing.CategoryBrowsingHandler;

/**
 *
 * @author  pawlowsk
 */
public class CategoryBrowsingHandlerFactory {
    
    /** Creates a new instance of CategoryBrowsingHandlerFactory */
    private CategoryBrowsingHandlerFactory() {
    }
    
    public static CategoryBrowsingHandler getHandler(ContainerServicesBase contSvcs) throws LaserException {
        CategoryBrowsingHandler handler = null;
        
        //if ( Constants.getDefaultWorkingMode() == Constants.TEST_WORKING_MODE )
        //    handler = CategoryHandler.get(); 
        //else 
            handler = CategoryBrowsingHandler.get(contSvcs);

        return handler;
    }
    
}

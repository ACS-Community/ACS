/*
 * DeleteAllFiltersAction.java
 *
 * Created on March 24, 2003, 4:34 PM
 */

package cern.laser.guiplatform.actions.filters;

import org.apache.log4j.Logger;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;

import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windows.configuration.ConsoleConfigurationWindow;

/**
 * This class removes all filters from filter list
 * @author  pawlowsk
 */
public class DeleteAllFiltersAction extends cern.gp.actions.support.NodeAction {

    protected static Logger logger = 
        LogFactory.getLogger(DeleteAllFiltersAction.class.getName());
    
    private final String deleteAllStr = 
        NbBundle.getMessage(DeleteAllFiltersAction.class,"LBL_DeleteAllFilterAction_name");
    
    /** Creates a new instance of DeleteAllFiltersAction */
    public DeleteAllFiltersAction() {
    }
    
    protected void performAction(cern.gp.nodes.GPNode[] gPNode) {
        TopComponent.Registry registry = TopComponent.getRegistry();
        
        ConsoleConfigurationWindow confWindow = 
            (ConsoleConfigurationWindow) registry.getActivated();
        
        if ( confWindow == null )
            throw new NullPointerException("Configuratrion window not found");
        
        confWindow.removeAllFilters();
        
    }
    
    public String getName() {
        return deleteAllStr;
    }
    
    protected String iconResource() {
        return "org/openide/resources/actions/cut.gif";
    }
    
}

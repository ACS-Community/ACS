/*
 * DeleteFilterAction.java
 *
 * Created on March 24, 2003, 3:48 PM
 */

package cern.laser.guiplatform.actions.filters;

import org.apache.log4j.Logger;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;

import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windows.configuration.ConsoleConfigurationWindow;

/**
 * This class removes filter from filters list
 * @author  pawlowsk
 */
public class DeleteFilterAction extends cern.gp.actions.support.NodeAction {

    protected static Logger logger = 
        LogFactory.getLogger(DeleteFilterAction.class.getName());
    
    private static final String deleteStr = 
        NbBundle.getMessage(DeleteFilterAction.class, "LBL_DeleteFilterAction_name");
    
    /** Creates a new instance of DeleteFilterAction */
    //public DeleteFilterAction() {
    //}
    
    protected void performAction(cern.gp.nodes.GPNode[] gPNode) {
    
        TopComponent.Registry registry = TopComponent.getRegistry();
        
        ConsoleConfigurationWindow confWindow = 
            (ConsoleConfigurationWindow) registry.getActivated();
        
        if ( confWindow == null )
            throw new NullPointerException("Configuratrion window not found");
        
        confWindow.removeFilter(gPNode[0]);
        
    }
    
    public String getName() {
        return deleteStr;
    }
    
    protected String iconResource() {
        return "org/openide/resources/actions/delete.gif";
    }

    protected boolean surviveFocusChange() {
        return false;
    }    
}

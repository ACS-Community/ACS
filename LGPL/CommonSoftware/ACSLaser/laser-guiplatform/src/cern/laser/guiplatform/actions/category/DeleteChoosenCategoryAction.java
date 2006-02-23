/*
 * DeleteChoosenCategoryAction.java
 *
 * Created on March 11, 2003, 4:23 PM
 */

package cern.laser.guiplatform.actions.category;

import java.beans.IntrospectionException;

import org.apache.log4j.Logger;
import org.openide.nodes.Node;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;

import cern.gp.nodes.GPNode;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windows.configuration.ConsoleConfigurationWindow;
import cern.laser.guiplatform.windows.search.CategorySelectorWindow;

/**
 * This addtion deletes one choosen category from choosen category explorer
 *
 * @author  pawlowsk
 */
public class DeleteChoosenCategoryAction extends cern.gp.actions.support.NodeAction {
    
    final static Logger logger =
    LogFactory.getLogger(DeleteChoosenCategoryAction.class.getName());
    
    private final String deleteStr = NbBundle.getMessage(
    DeleteChoosenCategoryAction.class,
    "LBL_DeleteChoosenCategoryAction_name");
    
    /** Creates a new instance of DeleteChoosenCategoryAction */
    //public DeleteChoosenCategoryAction() {
    //}
    
    /** the action will be enabled only if the super class enables it
     * and if the context is not null.
     * @see org.openide.util.actions.NodeAction#enable(Node[])
     *
     */
    protected boolean enable(Node[] activatedNodes) {
        return activatedNodes.length > 0 && super.enable(activatedNodes);
    }
    
    protected void performAction(GPNode[] activatedNodes) {
        for (int ix = 0; ix < activatedNodes.length; ix++)
            logger.debug(activatedNodes[ix].getName());
        
        TopComponent.Registry registry = TopComponent.getRegistry();
        
        if ( registry.getActivated() instanceof ConsoleConfigurationWindow ) {
            ConsoleConfigurationWindow confWindow =
            (ConsoleConfigurationWindow) registry.getActivated();
            
            if ( confWindow == null )
                throw new NullPointerException("Configuratrion window not found");
            
            for (int ix = 0; ix < activatedNodes.length; ix++) {
                try {
                    confWindow.removeCategory(activatedNodes[ix]);
                    confWindow.updateCategoryTreeExplorer();
                } catch (IntrospectionException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
                catch (CloneNotSupportedException e) {
                    // never happen
                    e.printStackTrace();
                }
            }
            
        }
        else {
            if ( registry.getActivated() instanceof CategorySelectorWindow ) {
                CategorySelectorWindow confWindow =
                (CategorySelectorWindow) registry.getActivated();
                
                if ( confWindow == null )
                    throw new NullPointerException("Configuratrion window not found");
                
                for (int ix = 0; ix < activatedNodes.length; ix++) {
                    try {
                        confWindow.removeCategory(activatedNodes[ix]);
                        confWindow.updateCategoryTreeExplorer();
                    } catch (IntrospectionException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
                    catch (CloneNotSupportedException e) {
                        // never happen
                        e.printStackTrace();
                    }
                }
            }
            else{
                throw new UnsupportedOperationException();
            }
        }
        
    }
    
    /**
     */
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

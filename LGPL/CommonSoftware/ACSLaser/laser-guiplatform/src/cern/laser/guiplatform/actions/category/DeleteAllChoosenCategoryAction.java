/*
 * DeleteAllChoosenCategoryAction.java
 *
 * Created on March 12, 2003, 12:05 PM
 */

package cern.laser.guiplatform.actions.category;

import java.beans.IntrospectionException;

import org.openide.nodes.Node;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;

import cern.gp.nodes.GPNode;
import cern.laser.guiplatform.windows.configuration.ConsoleConfigurationWindow;
import cern.laser.guiplatform.windows.search.CategorySelectorWindow;

/**
 * This action deletes all choosen category from choosen category explorer
 *
 * @author  pawlowsk
 */
public class DeleteAllChoosenCategoryAction extends cern.gp.actions.support.NodeAction {
    
    private final String deleteAllStr = NbBundle.getMessage(
    DeleteAllChoosenCategoryAction.class,
    "LBL_DeleteAllChoosenCategoryAction_name");
    
    /** Creates a new instance of DeleteAllChoosenCategoryAction */
    public DeleteAllChoosenCategoryAction() {
    }
    
    /** the action will be enabled only if the super class enables it
     * and if the context is not null.
     * @see org.openide.util.actions.NodeAction#enable(Node[])
     *
     */
    protected boolean enable(Node[] nodes) {
        return super.enable(nodes);
    }
    
    /** A method called by GP when you invoke this action. You have to implement this
     * method with the action you want to execute. Please note that the context parameter
     * may be null.
     *
     * @param activatedNodes the nodes currently selected
     * @param context the object set with {@link setContext()} can be null
     *
     */
    protected void performAction(GPNode[] activatedNodes, Object context) {
        /*
        if (context instanceof ChoosenCategoryExplorer) {
            ChoosenCategoryExplorer exp = (ChoosenCategoryExplorer) context;
            exp.removeAllCategories();
        }
         */
    }
    protected void performAction(GPNode[] activatedNodes) {
        TopComponent.Registry registry = TopComponent.getRegistry();
        
        if ( registry.getActivated() instanceof ConsoleConfigurationWindow ) {
            ConsoleConfigurationWindow confWindow =
            (ConsoleConfigurationWindow) registry.getActivated();
            
            if ( confWindow == null )
                throw new NullPointerException("Configuratrion window not found");
            try {
                confWindow.removeAllCategories();
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
        else {
            if ( registry.getActivated() instanceof CategorySelectorWindow ) {
                CategorySelectorWindow confWindow =
                (CategorySelectorWindow) registry.getActivated();
                
                if ( confWindow == null )
                    throw new NullPointerException("Configuratrion window not found");
                try {
                    confWindow.removeAllCategories();
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
            else {
                throw new UnsupportedOperationException();
            }
        }
        
    }
    /**
     */
    public String getName() {
        return deleteAllStr;
    }
    
    protected String iconResource() {
        return "org/openide/resources/actions/cut.gif";
    }
}

/*
 * CategoryCopyOverNodesAction.java
 *
 * Created on March 7, 2003, 2:29 PM
 */

package cern.laser.guiplatform.actions.category;

import java.beans.IntrospectionException;

import org.apache.log4j.Logger;
import org.openide.nodes.Node;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;

import cern.gp.actions.support.NodeAction;
import cern.gp.nodes.GPNode;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windows.configuration.ConsoleConfigurationWindow;
import cern.laser.guiplatform.windows.search.CategorySelectorWindow;

/**
 * This class copies BeanSupport objects from TreeExplorer to ListExplorer.
 * (from CateogoryTree to ChoosenCategories)
 *
 * @author  pawlowsk
 */
public class CategoryCopyOverNodesAction extends NodeAction {
    
    protected static final Logger logger =
    LogFactory.getLogger(CategoryCopyOverNodesAction.class.getName());
    
    private static final String addStr = NbBundle.getMessage(
    CategoryCopyOverNodesAction.class,
    "LBL_CategoryCopyOverNodesAction_name");
    
    /** Creates a new instance of CategoryCopyOverNodesAction */
    //public CategoryCopyOverNodesAction() {
    //}
    
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
            for (int ix = 0; ix < activatedNodes.length; ix++)
                try {
                    GPNode gpNode = activatedNodes[ix];
                    exp.addCategory(gpNode);
                } catch (IntrospectionException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                } catch (CloneNotSupportedException e) {
                    // never happen
                    e.printStackTrace();
                }
         
         
        }
         */
    }
    
    public String getName() {
        return addStr;
    }
    
    protected String iconResource() {
        return "org/openide/resources/actions/save.gif";
    }
    
    protected void performAction(cern.gp.nodes.GPNode[] activatedNodes) {
        
        TopComponent.Registry registry = TopComponent.getRegistry();
        
        logger.debug("registry=" + registry+ " activated="+registry.getActivated().getClass().getName());
        
        if ( registry.getActivated() instanceof ConsoleConfigurationWindow ) {
            
            ConsoleConfigurationWindow confWindow =
            (ConsoleConfigurationWindow) registry.getActivated();
            
            logger.debug("confWindow=" + confWindow);
            
            if ( confWindow == null )
                throw new NullPointerException("Configuratrion window not found");
            
            
            for (int ix = 0; ix < activatedNodes.length; ix++) {
                try {
                    GPNode gpNode = activatedNodes[ix];
                    confWindow.addCategory(gpNode);
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
                
                logger.debug("confWindow=" + confWindow);
                
                if ( confWindow == null )
                    throw new NullPointerException("Configuratrion window not found");
                
                
                for (int ix = 0; ix < activatedNodes.length; ix++) {
                    try {
                        GPNode gpNode = activatedNodes[ix];
                        confWindow.addCategory(gpNode);
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
                throw new UnsupportedOperationException();
            }
        }
    }
    
    protected boolean surviveFocusChange() {
        return false;
    }
    
}

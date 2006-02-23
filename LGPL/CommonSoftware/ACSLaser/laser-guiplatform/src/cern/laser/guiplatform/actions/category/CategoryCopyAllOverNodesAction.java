/*
 * CategoryCopyAllOverNodesAction.java
 *
 * Created on March 13, 2003, 4:00 PM
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
 *
 * @author  pawlowsk
 */
public class CategoryCopyAllOverNodesAction extends cern.gp.actions.support.NodeAction {
    
    protected static final Logger logger =
    LogFactory.getLogger(CategoryCopyAllOverNodesAction.class.getName());
    
    private static final String addAllStr = NbBundle.getMessage(
    CategoryCopyAllOverNodesAction.class,
    "LBL_CategoryCopyAllOverNodesAction_name");
    
    
    /** Creates a new instance of CategoryCopyAllOverNodesAction */
    public CategoryCopyAllOverNodesAction() {
    }
    
    /** the action will be enabled only if the super class enables it
     * and if the context is not null.
     * @see org.openide.util.actions.NodeAction#enable(Node[])
     *
     */
    protected boolean enable(Node[] nodes) {
        return super.enable(nodes);
    }
    
    
    public String getName() {
        return addAllStr;
    }
    
    protected String iconResource() {
        return "org/openide/resources/actions/saveAll.gif";
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
            GPNode rootNode = activatedNodes[0];
            while( rootNode.getParent() != null ) {
                rootNode = rootNode.getParent();
            }
            try {
                exp.addCategory(rootNode);
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
    
    protected void performAction(GPNode[] activatedNodes) {
        
        TopComponent.Registry registry = TopComponent.getRegistry();
        
        if ( registry.getActivated() instanceof ConsoleConfigurationWindow ) {
            
            ConsoleConfigurationWindow confWindow =
            (ConsoleConfigurationWindow) registry.getActivated();
            
            if ( confWindow == null )
                throw new NullPointerException("Configuratrion window not found");
            
            GPNode rootNode = activatedNodes[0];
            while( rootNode.getParent() != null ) {
                rootNode = rootNode.getParent();
            }
            try {
                confWindow.addCategory(rootNode);
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
                
                GPNode rootNode = activatedNodes[0];
                while( rootNode.getParent() != null ) {
                    rootNode = rootNode.getParent();
                }
                try {
                    confWindow.addCategory(rootNode);
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
}

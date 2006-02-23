/*
 * EditFilterAction.java
 *
 * Created on March 24, 2003, 12:16 PM
 */

package cern.laser.guiplatform.actions.filters;

import org.apache.log4j.Logger;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;

import cern.laser.guiplatform.filters.FilterItemBean;
import cern.laser.guiplatform.windows.configuration.ConsoleConfigurationWindow;

/**
 * This action allows to edit previously defined filters
 * 
 * @author  pawlowsk
 */
public class EditFilterAction extends cern.gp.actions.support.NodeAction {

    static Logger logger = Logger.getLogger(
                                EditFilterAction.class.getName());
    
    private static final String editStr = 
        NbBundle.getMessage(EditFilterAction.class, "LBL_EditFilterAction_name");
    
    /** Creates a new instance of EditFilterAction */
    //public EditFilterAction() {
    //}
    
    protected void performAction(cern.gp.nodes.GPNode[] gPNode) {
        
        TopComponent.Registry registry = TopComponent.getRegistry();
        
        ConsoleConfigurationWindow confWindow = 
            (ConsoleConfigurationWindow) registry.getActivated();
        
        if ( confWindow == null )
            throw new NullPointerException("Configuratrion window not found");
        
        FilterItemBean filterBean = (FilterItemBean) gPNode[0].getBean(); 
        
        
        //confWindow.removeAllFilters();
 
        confWindow.setInUpdateMode(filterBean.getName(), filterBean.getOperator(),
                          filterBean.getValue(), "Edit filter");
        
 


    }
    
    protected boolean enable(org.openide.nodes.Node[] node) {
        // TODO: enable if choosen only one node
        return super.enable(node) && node.length == 1;
    }
    
    public String getName() {
        return editStr;
    }
    
    protected String iconResource() {
        return "org/openide/resources/editorMode.gif";
    }

    
    
}

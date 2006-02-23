/*
 * InhibitAction.java
 *
 * Created on May 23, 2003, 11:36 AM
 */

package cern.laser.guiplatform.actions.alarms;

import org.apache.log4j.Logger;
import org.openide.nodes.Node;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

import cern.laser.guiplatform.alarms.AlarmBean;
import cern.laser.guiplatform.alarms.AlarmBeanNode;
import cern.laser.guiplatform.alarms.AlarmContainer;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;

/**
 *
 * @author  pawlowsk
 */
public class UnhighlightedAndKlaxonAction extends org.openide.util.actions.NodeAction {
    
    /** action name */
    private static final String name =
    NbBundle.getMessage(UnhighlightedAndKlaxonAction.class,
    "LBL_Action_UnhighlightAndKlaxon_action_name");
    
    /** logger */
    private static final Logger logger =
    LogFactory.getLogger(UnhighlightedAndKlaxonAction.class.getName());
    
    
    /** Creates a new instance of InhibitAction */
    //public UnhighlightedAndKlaxonAction() {
    //}
    
    public String getName() {
        return name;
    }
    
    protected String iconResource() {
        return null;
    }
    /*
    protected void performCapability(GPNode gPNode, Capability capability) {
        // move gpNode from active list to inhibit list
        DeleteCapability deleteCapability = (DeleteCapability) capability;
        deleteCapability.delete(node);
        logger.debug(gPNode.getName() + " has been inhibited ");
     
    }
     */
    protected boolean enable(org.openide.nodes.Node[] node) {
        boolean result = false;
        
        for (int i = 0; i < node.length; i++) {
            try {
                AlarmBean alarm = (AlarmBean) ((AlarmBeanNode)node[i]).getBean();
                if ( alarm.isAlarmNodeHighlightedAndKlaxon() ) {
                    result = true;
                    break;
                }
            }
            catch(ClassCastException cce) {
                logger.debug("ClassCastException obj="+node[i]);
                return false;
            }
        }
        return result;
        
        
    }
    
    protected void performAction( Node [] activatedNodes) {
        
        AcWindowManager.setStatusText("Unhighlight And Klaxon action is running .......");
        AlarmBean [] unhighlightedAndKlaxoned = new AlarmBean[activatedNodes.length];
        for (int i = 0; i < activatedNodes.length; i++)
            unhighlightedAndKlaxoned[i] = (AlarmBean) ((AlarmBeanNode)activatedNodes[i]).getBean();
        
        AlarmContainer.getDefault().unhighlightAndKlaxon(unhighlightedAndKlaxoned);
        
        
        AcWindowManager.setZeroSelectedNodes(
        org.openide.util.NbBundle.getMessage(
        cern.laser.guiplatform.windows.ActiveListExplorerPanel.class,
        "LBL_HighligtedAndKlaxon_list_component_name"));
        
        AcWindowManager.setStatusText("Unhighlight And Klaxon action finished");
        
    }
    
    public org.openide.util.HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
    }
    
}

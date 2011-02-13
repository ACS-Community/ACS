/*
 * UninhibitAction.java
 *
 * Created on May 23, 2003, 3:22 PM
 */

package cern.laser.guiplatform.actions.alarms;

//import cern.gp.nodes.GPNode;
import org.apache.log4j.Logger;
import org.openide.nodes.Node;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

import alma.acs.container.ContainerServicesBase;

import cern.laser.guiplatform.alarms.AlarmBean;
import cern.laser.guiplatform.alarms.AlarmBeanNode;
import cern.laser.guiplatform.alarms.AlarmContainer;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;
/**
 *
 * @author  pawlowsk
 */
public class UninhibitAction extends org.openide.util.actions.NodeAction {
    
    /** action name */
    private static final String name = 
        NbBundle.getMessage(UninhibitAction.class, "LBL_Action_Uninhibit_action_name");;
    
    /** logger */
    private static final Logger logger = 
                LogFactory.getLogger(UninhibitAction.class.getName());;
                
    private final ContainerServicesBase contSvcs;
    
    /** Creates a new instance of UninhibitAction */
    public UninhibitAction(ContainerServicesBase contSvcs) {
    	this.contSvcs=contSvcs;
    }
    
    protected String iconResource() {
        return null;
    }
    
    public String getName() {
        return name;
    }
    
    /*
    protected void performCapability(GPNode gPNode, Capability capability) {
        // move gpNode from active list to inhibit list
        
        logger.debug(gPNode.getName() + " has been inhibited ");
        
    }
    */
    
    protected boolean enable(org.openide.nodes.Node[] node) {
        boolean result = false;

        for (int i = 0; i < node.length; i++) {
            try {
                AlarmBean alarm = (AlarmBean) ((AlarmBeanNode)node[i]).getBean();
                
                logger.debug("inhibited="+alarm.isAlarmNodeInhibited() );
                logger.debug("on highlighted list="+alarm.isOnHighlightedList() );
                
                if ( alarm.isAlarmNodeInhibited()&&!alarm.isOnHighlightedList()) {                    
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

    protected void performAction(Node [] activatedNodes) {
    
        AcWindowManager.setStatusText("Uninhibit action is running ..........");
        AlarmBean [] uninhibitedAlarms = new AlarmBean[activatedNodes.length];
        for (int i = 0; i < activatedNodes.length; i++) 
            uninhibitedAlarms[i] = (AlarmBean) ((AlarmBeanNode)activatedNodes[i]).getBean();
        
        AlarmContainer.getDefault().uninhibit(uninhibitedAlarms,contSvcs);

        AcWindowManager.setZeroSelectedNodes(
                org.openide.util.NbBundle.getMessage(
                                    cern.laser.guiplatform.windows.ActiveListExplorerPanel.class, 
                                   "LBL_Inhibit_list_component_name")); 
        AcWindowManager.setStatusText("Uninhibit action finished");

    } 

    public org.openide.util.HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
    }    
    
}

/*
 * MaskAction.java
 *
 * Created on May 26, 2003, 6:20 PM
 */

package cern.laser.guiplatform.actions.alarms;

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
public class UnmaskAction extends org.openide.util.actions.NodeAction {
    
    /** action name */
    private static final String name = 
        NbBundle.getMessage(MaskAction.class, "LBL_Action_Unmask_action_name");
    
    /** logger */
    private static final Logger logger = 
                            LogFactory.getLogger(UnmaskAction.class.getName());
    
    private final ContainerServicesBase contSvcs;
    
    /** Creates a new instance of MaskAction */
    public UnmaskAction(ContainerServicesBase contSvcs) {
    	this.contSvcs=contSvcs;
    }
    
    //protected void performCapability(cern.gp.nodes.GPNode gPNode, cern.gp.capabilities.Capability capability) {
    //}
    
    public String getName() {
        return name;
    }
    
    protected String iconResource() {
        return null;
    }
      
    protected boolean enable(org.openide.nodes.Node[] node) {
        boolean result = false;
        /*
        org.openide.nodes.Node.Cookie cookie = null;//
        if ( node.length > 0 ) {
            cookie = node[0].getCookie(AlarmBean.class);
            if ( cookie != null ) 
                result = ((AlarmBean) cookie).isAlarmNodeMasked() &&
                    !((AlarmBean) cookie).isOnHighlightedList();
        }
        return super.enable(node) && result;
        */
        
        for (int i = 0; i < node.length; i++) {
            try {
                AlarmBean alarm = (AlarmBean) ((AlarmBeanNode)node[i]).getBean();
                if ( alarm.isAlarmNodeMasked()&&!alarm.isOnHighlightedList()) {                    
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

        AlarmBean [] unmaskedAlarms = new AlarmBean[activatedNodes.length];
        for (int i = 0; i < activatedNodes.length; i++) 
            unmaskedAlarms[i] = (AlarmBean) ((AlarmBeanNode)activatedNodes[i]).getBean();
        
        AlarmContainer.getDefault().unmask(unmaskedAlarms,contSvcs);
        
        AcWindowManager.setZeroSelectedNodes(org.openide.util.NbBundle.getMessage(
                                            cern.laser.guiplatform.windows.ActiveListExplorerPanel.class, 
                                            "LBL_Mask_list_component_name"));

    } 

    public org.openide.util.HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
    }    
  
}

/*
 * InhibitAction.java
 *
 * Created on May 23, 2003, 11:36 AM
 */

package cern.laser.guiplatform.actions.alarms;

//import cern.gp.nodes.GPNode;
import org.apache.log4j.Logger;
import org.openide.DialogDescriptor;
import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.nodes.Node;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

import cern.laser.console.Comment;
import cern.laser.guiplatform.alarms.AlarmBean;
import cern.laser.guiplatform.alarms.AlarmBeanNode;
import cern.laser.guiplatform.alarms.AlarmContainer;
import cern.laser.guiplatform.util.AppRegister;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;
import cern.laser.guiplatform.windows.ActiveListExplorerPanel;
import cern.laser.guiplatform.windows.alarms.AlarmCommentPanel;

/**
 *
 * @author  pawlowsk
 */
//public class InhibitAction extends cern.gp.actions.support.BeanActionSupport {
public class InhibitAction extends org.openide.util.actions.NodeAction {
    
    /** action name */
    private static final String name = 
        NbBundle.getMessage(InhibitAction.class, "LBL_Action_Inhibit_action_name");
    
    /** logger */
    private static final Logger logger = 
                            LogFactory.getLogger(InhibitAction.class.getName());
    
    
    /** Creates a new instance of InhibitAction */
    //public InhibitAction() {
        //super(InhibitCapability.class);
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
        
        /*
        org.openide.nodes.Node.Cookie cookie = null;//
        if ( node.length > 0 ) {
            cookie = node[0].getCookie(AlarmBean.class);
            if ( cookie != null )
            result = ((AlarmBean) cookie).isAlarmNodeActive() &&
                !((AlarmBean) cookie).isOnHighlightedList();
        }
        return result && super.enable(node);
         */
        
        for (int i = 0; i < node.length; i++) {
            try {
                AlarmBeanNode abnode = (AlarmBeanNode) node[i];
                logger.debug("enable(), alarmBeanNode has bean="+abnode.getBean() );
                
                AlarmBean alarm = (AlarmBean) abnode.getBean();
                
                logger.debug("active="+alarm.isAlarmNodeActive() );
                logger.debug("on highlighted list="+alarm.isOnHighlightedList() );
                
                if ( alarm.isAlarmNodeActive() && !alarm.isOnHighlightedList()) {                    
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
        String operatorName = 
            AppRegister.getInstance().getRegisteredUserName();
        String commnetText = null;

        // prepare comment
        DialogDescriptor commentDesc = 
            new DialogDescriptor(new AlarmCommentPanel(operatorName, commnetText),
                                 NbBundle.getMessage(AlarmCommentPanel.class,
                                        "LBL_COMMENT_ON_INHIBITED_ALARM_display_name"));

        
        if ( DialogDisplayer.getDefault().notify(commentDesc) != NotifyDescriptor.OK_OPTION ) {
             return;
        }
            /*
            TraceLogger.log("user decided not to inhibit this alarm: " + 
                            alarm.getFaultFamily() + " " + alarm.getFaultMember() +
                            " " + alarm.getFaultCode());
           */
        AcWindowManager.setStatusText("Inhibit action is running .........");

        AlarmCommentPanel commentPanel = (AlarmCommentPanel) commentDesc.getMessage(); 
        Comment comment = new Comment(commentPanel.getOperatorName(), 
                                       commentPanel.getCommnet());

        AlarmBean [] inhibitedAlarms = new AlarmBean[activatedNodes.length];
        for (int i = 0; i < activatedNodes.length; i++) 
            inhibitedAlarms[i] = (AlarmBean) ((AlarmBeanNode)activatedNodes[i]).getBean();
        
        AlarmContainer.getDefault().inhibit(inhibitedAlarms, comment);

        ActiveListExplorerPanel activeList = (ActiveListExplorerPanel)
            AcWindowManager.findTopComponent(
                org.openide.util.NbBundle.getMessage(cern.laser.guiplatform.windows.ActiveListExplorerPanel.class, 
                                   "LBL_Active_list_component_name"));
        
        if ( activeList != null )  {
            logger.debug("inhibit list panel found");
            try {
                activeList.getExplorerManager().setSelectedNodes(new org.openide.nodes.Node[0]);
            } catch (java.beans.PropertyVetoException pve) {
                logger.error(pve, pve.fillInStackTrace());
            }
        }
        AcWindowManager.setStatusText("Inhibit action finished");
  
    } 

    public org.openide.util.HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
    }    

}

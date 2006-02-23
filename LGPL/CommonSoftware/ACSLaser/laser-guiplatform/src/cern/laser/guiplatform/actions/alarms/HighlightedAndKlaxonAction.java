/*
 * InhibitAction.java
 *
 * Created on May 23, 2003, 11:36 AM
 */

package cern.laser.guiplatform.actions.alarms;

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
import cern.laser.guiplatform.windows.alarms.AlarmCommentPanel;
import cern.laser.guiplatform.windows.alarms.ChooseHighlightOrKlaxonPanel;

/**
 *
 * @author  pawlowsk
 */
public class HighlightedAndKlaxonAction extends org.openide.util.actions.NodeAction {
    
    /** action name */
    private static final String name = 
        NbBundle.getMessage(HighlightedAndKlaxonAction.class, "LBL_Action_HighlightAndKlaxon_action_name");
    
    /** logger */
    private static final Logger logger = 
                            LogFactory.getLogger(HighlightedAndKlaxonAction.class.getName());
    
    
    /** Creates a new instance of InhibitAction */
    //public HighlightedAndKlaxonAction() {
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
        org.openide.nodes.Node.Cookie cookie = null;
        //if ( node.length > 0 ) {
        for (int i = 0; i < node.length; i++) { 
            cookie = node[i].getCookie(AlarmBean.class);
            if ( //!((AlarmBean) cookie).isAlarmNodeActive() ||
                 cookie != null &&
                 ((AlarmBean) cookie).isAlarmNodeHighlightedAndKlaxon() ) {
                result = false; 
                break;
            }
        }
        return result && super.enable(node);
         */
        
        for (int i = 0; i < node.length; i++) {
            try {
                AlarmBean alarm = (AlarmBean) ((AlarmBeanNode)node[i]).getBean();
                if ( !alarm.isAlarmNodeHighlightedAndKlaxon() ) {                    
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
        // prepare window where user can choose beetwen klaxon 
        // or highlight
        // nofity descriptor (choose highligh or klaxon)
        DialogDescriptor dialog = new DialogDescriptor(new
                ChooseHighlightOrKlaxonPanel(),
                NbBundle.getMessage(ChooseHighlightOrKlaxonPanel.class,
                    "LBL_HIGHLIGHT_OR_KLAXON_dialog_title"));
        DialogDisplayer.getDefault().notify(dialog);
        
        //if ( dialog.getValue().equals(NotifyDescriptor.CANCEL_OPTION) )
        //    System.out.println("Cancel was choosen!");

        if ( !dialog.getValue().equals(NotifyDescriptor.OK_OPTION) )
           return; 
 
        // prepare comment
        String operatorName = 
            AppRegister.getInstance().getRegisteredUserName();
        String commnetText = null;

        DialogDescriptor commentDesc = 
            new DialogDescriptor(new AlarmCommentPanel(operatorName, commnetText),
                                 NbBundle.getMessage(
                                     AlarmCommentPanel.class,
                                     "LBL_COMMENT_ON_HIGHLIGHTED_AND_KLAXON_ALARM_display_name"));
        
        if ( DialogDisplayer.getDefault().notify(commentDesc) != NotifyDescriptor.OK_OPTION ) {
             return;
        }
        
        AcWindowManager.setStatusText("Highlight And Klaxon action is running .......");

        AlarmCommentPanel commentPanel = (AlarmCommentPanel) commentDesc.getMessage(); 
        Comment comment = new Comment(commentPanel.getOperatorName(), 
                                       commentPanel.getCommnet());

        AlarmBean [] highlightedAndKlaxonedAlarms = new AlarmBean[activatedNodes.length];
        for (int i = 0; i < activatedNodes.length; i++) 
            highlightedAndKlaxonedAlarms[i] = (AlarmBean) ((AlarmBeanNode)activatedNodes[i]).getBean();
            
        ChooseHighlightOrKlaxonPanel panel = 
            (ChooseHighlightOrKlaxonPanel) dialog.getMessage(); 
       
        int highlightOption = panel.getHighlightedOrKlaxon();
        AlarmContainer.getDefault().highlightAndKlaxon(
                        highlightedAndKlaxonedAlarms, comment, highlightOption);


        // TODO highlightAndKlaxon topComponent set selectednodes(new
        // Node[9]);
        AcWindowManager.setStatusText("Highlight And Klaxon action finished");

    }

    public org.openide.util.HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
    }    
 
}

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
import cern.laser.guiplatform.windows.alarms.AlarmCommentPanel;


/** Action sensitive to the node selection that does something useful.
 * Consider using a cookie action instead if you can define what the
 * action is applicable to in terms of cookies.
 * @author Bartlomiej Pawlowski <Bartlomiej.Pawlowski@cern.ch>
 */
public class AcknowledgeAction extends org.openide.util.actions.NodeAction {
    
    /** action name */
    private static final String name =
    NbBundle.getMessage(AcknowledgeAction.class, "LBL_AcknowledgeAction_action_name");
    
    /** logger */
    private static final Logger logger =
    LogFactory.getLogger(AcknowledgeAction.class.getName());
    
    protected boolean enable(org.openide.nodes.Node[] node) {
        
    	if (node.length == 0)
    		return false;
    	
    	boolean result = true;

        for (int i = 0; i < node.length; i++) {
            try {
                AlarmBean alarm = (AlarmBean) ((AlarmBeanNode)node[i]).getBean();
                if ( !alarm.isAlarmNodeActive() ||
                alarm.isAlarmNodeAcknowledged() ||
                alarm.isOnHighlightedList() ) {                    
                    result = false;
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
    
    
    public String getName() {
        return name;
    }
    
    protected String iconResource() {
        //return "cern/laser/guiplatform/actions/alarms/AcknowledgeActionActionIcon.gif";
        return null;
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(AcknowledgeActionAction.class);
    }
    
    protected void performAction( Node [] activatedNodes) {
        // prepare comment
        
        String operatorName =
        AppRegister.getInstance().getRegisteredUserName();
        String commnetText = null;
        
        DialogDescriptor commentDesc =
        new DialogDescriptor(new AlarmCommentPanel(operatorName, commnetText),
        NbBundle.getMessage(
        AlarmCommentPanel.class,
        "LBL_COMMENT_ON_ACKNOWLEDGED_ALARM_display_name"));
        
        
        if ( DialogDisplayer.getDefault().notify(commentDesc) !=
        NotifyDescriptor.OK_OPTION ) {
            return;
        }
        
        AcWindowManager.setStatusText("Acknowledge action is running .......");
        AlarmCommentPanel commentPanel = (AlarmCommentPanel) commentDesc.getMessage();
        Comment comment = new Comment(commentPanel.getOperatorName(),
        commentPanel.getCommnet());
        
        
        AlarmBean [] acknowledgeddAlarms = new AlarmBean[activatedNodes.length];
        for (int i = 0; i < activatedNodes.length; i++)
            acknowledgeddAlarms[i] = (AlarmBean) ((AlarmBeanNode)activatedNodes[i]).getBean();
        
        
        
        AlarmContainer.getDefault().acknowledge(acknowledgeddAlarms, true, comment);
        AcWindowManager.setStatusText("Acknowledge action finished");
        
    }
    
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(AcknowledgeActionAction.class, "HINT_Action"));
     * }
     */
    
}

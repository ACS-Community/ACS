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

/** Action sensitive to the node selection that does something useful.
 * Consider using a cookie action instead if you can define what the
 * action is applicable to in terms of cookies.
 * @author Bartlomiej Pawlowski <Bartlomiej.Pawlowski@cern.ch>
 */
public class UnacknowledgeAction extends org.openide.util.actions.NodeAction {
    
   
    /** action name */
    private static final String name = 
        NbBundle.getMessage(UnacknowledgeAction.class, "LBL_UnacknowledgeAction_action_name");

    /** logger */
    private static final Logger logger = 
        LogFactory.getLogger(UnacknowledgeAction.class.getName());
   
    public String getName() {
        return name;
    
    }
   
    protected boolean enable(org.openide.nodes.Node[] node) {
        boolean result = false;
        
        for (int i = 0; i < node.length; i++) {
            try {
                AlarmBean alarm = (AlarmBean) ((AlarmBeanNode)node[i]).getBean();
                if ( alarm.isAlarmNodeAcknowledged() &&
                !alarm.isOnHighlightedList() &&
                !alarm.isAlarmNodeInhibited() ) {                    
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

        AcWindowManager.setStatusText("Unacknowledge action is running .......");
        AlarmBean [] acknowledgeddAlarms = new AlarmBean[activatedNodes.length];
        for (int i = 0; i < activatedNodes.length; i++) 
            acknowledgeddAlarms[i] = (AlarmBean) ((AlarmBeanNode)activatedNodes[i]).getBean();
        
        AlarmContainer.getDefault().acknowledge(acknowledgeddAlarms, false, null);
        AcWindowManager.setStatusText("Unacknowledge action finished");
 
    } 
 
    protected String iconResource() {
        //return "cern/laser/guiplatform/actions/alarms/UnacknowledgeActionIcon.gif";
        return null;
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(UnacknowledgeAction.class);
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(UnacknowledgeAction.class, "HINT_Action"));
     * }
     */
    
}

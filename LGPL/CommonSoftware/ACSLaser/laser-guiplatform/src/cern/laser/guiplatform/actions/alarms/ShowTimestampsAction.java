package cern.laser.guiplatform.actions.alarms;

//import cern.gp.nodes.GPNode;
import org.apache.log4j.Logger;
import org.openide.nodes.Node;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

import cern.laser.guiplatform.alarms.AlarmBean;
import cern.laser.guiplatform.alarms.AlarmBeanNode;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;

/** Action sensitive to the node selection that does something useful.
 * Consider using a cookie action instead if you can define what the
 * action is applicable to in terms of cookies.
 * @author Bartlomiej Pawlowski <Bartlomiej.Pawlowski@cern.ch>
 */
public class ShowTimestampsAction extends org.openide.util.actions.NodeAction {

   /** action name */
    private static final String name = 
        NbBundle.getMessage(ShowTimestampsAction.class, 
                "LBL_Action_ShowTimestamps_action_name");
    
    /** logger */
    private static final Logger logger = 
                LogFactory.getLogger(ShowTimestampsAction.class.getName());
  
    protected void performAction( Node [] activatedNodes) {
        AlarmBean ab = (AlarmBean) ((AlarmBeanNode)activatedNodes[0]).getBean();
        AcWindowManager.showTimestamps(ab);
    }
    
    protected boolean enable(Node[] node) {
        boolean result = false;
        /*
        org.openide.nodes.Node.Cookie cookie = null;//
        if ( nodes.length > 0 ) {
            cookie = nodes[0].getCookie(AlarmBean.class);
            // cookie in final version should always != null
            if ( cookie != null ) {
                result = ((AlarmBean) cookie).isAlarmNodeActive() &&
                    !((AlarmBean) cookie).isOnHighlightedList();
            }
        }
        return result && super.enable(nodes);
         */
        
        for (int i = 0; i < node.length; i++) {
            try {
                AlarmBean alarm = (AlarmBean) ((AlarmBeanNode)node[i]).getBean();
                if ( alarm.isAlarmNodeActive() && !alarm.isOnHighlightedList() ) {                    
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
    
    public String getName() {
        return name;
    }
    
    protected String iconResource() {
        //return "cern/laser/guiplatform/actions/alarms/ShowTimestampsActionIcon.gif";
        return null;
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(ShowTimestampsAction.class);
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(ShowTimestampsAction.class, "HINT_Action"));
     * }
     */
    
}

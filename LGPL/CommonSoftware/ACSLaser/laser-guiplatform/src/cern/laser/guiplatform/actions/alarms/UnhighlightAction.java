package cern.laser.guiplatform.actions.alarms;

//import cern.gp.nodes.GPNode;
import org.apache.log4j.Logger;
import org.openide.nodes.Node;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

import cern.laser.guiplatform.alarms.AlarmBean;
import cern.laser.guiplatform.alarms.AlarmBeanNode;
import cern.laser.guiplatform.alarms.AlarmContainer;
import cern.laser.guiplatform.util.LogFactory;

/** 
 * This action unhighlights (only unhighlights) alarms
 *
 * Action sensitive to the node selection that does something useful.
 * Consider using a cookie action instead if you can define what the
 * action is applicable to in terms of cookies.
 * @author Bartlomiej Pawlowski <Bartlomiej.Pawlowski@cern.ch>
 */
public class UnhighlightAction extends org.openide.util.actions.NodeAction {
    
     /** action name */
    private static final String name = 
        NbBundle.getMessage(UnhighlightAction.class, "LBL_Action_Unhighlight_action_name");

    /** logger */
    private static final Logger logger = 
                            LogFactory.getLogger(UnhighlightAction.class.getName());
  
    protected boolean enable(org.openide.nodes.Node[] node) {
        
    	if (node.length == 0)
    		return false;
    	
    	boolean result = true;
        
        for (int i = 0; i < node.length; i++) {
            try {
                AlarmBean alarm = (AlarmBean) ((AlarmBeanNode)node[i]).getBean();
                if ( !alarm.isAlarmNodeActive()||!alarm.isAlarmNodeHighlighted()) {                    
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
        return null;
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(UnhighlightAction.class);
    }
   
    protected void performAction(Node [] activatedNodes) {

        AlarmBean [] highlightedAlarms = new AlarmBean[activatedNodes.length];
        for (int i = 0; i < activatedNodes.length; i++) 
            highlightedAlarms[i] = (AlarmBean) ((AlarmBeanNode)activatedNodes[i]).getBean();
        
        AlarmContainer.getDefault().highlight(highlightedAlarms, false);
 
    } 

    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(UnhighlightAction.class, "HINT_Action"));
     * }
     */
    
}

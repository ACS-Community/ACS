/*
 * InhibitAction.java
 *
 * Created on May 23, 2003, 11:36 AM
 */

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
 * This action only highlights alarm (this is not persisted)
 * @author  pawlowsk
 */
public class HighlightAction extends org.openide.util.actions.NodeAction {
    
    /** action name */
    private static final String name =
    NbBundle.getMessage(HighlightAction.class, "LBL_Action_Highlight_action_name");
    
    /** logger */
    private static final Logger logger =
    LogFactory.getLogger(HighlightAction.class.getName());
    
    
    public String getName() {
        return name;
    }
    
    protected String iconResource() {
        return null;
    }
    
    protected boolean enable(org.openide.nodes.Node[] node) {

    	if (node.length == 0)
    		return false;    	
    	
    	boolean result = true;
        /*
        org.openide.nodes.Node.Cookie cookie = null;
        for (int i = 0; i < node.length; i++) {
            cookie = node[i].getCookie(AlarmBean.class);
            if ( //!((AlarmBean) cookie).isAlarmNodeActive() ||
                 cookie != null &&
                 ((AlarmBean) cookie).isAlarmNodeHighlighted() ) {
                result = false;
                break;
            }
        }
        return result && super.enable(node);
         */
        for (int i = 0; i < node.length; i++) {
            try {
                AlarmBean alarm = (AlarmBean) ((AlarmBeanNode)node[i]).getBean();
                if ( alarm.isAlarmNodeHighlighted() ) {
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
    
    protected void performAction(Node [] activatedNodes) {
        
        AlarmBean [] highlightedAlarms = new AlarmBean[activatedNodes.length];
        for (int i = 0; i < activatedNodes.length; i++)
            highlightedAlarms[i] = (AlarmBean) ((AlarmBeanNode) activatedNodes[i]).getBean();
        
        AlarmContainer.getDefault().highlight(highlightedAlarms, true);
        
    }
    
    public org.openide.util.HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
    }
    
}

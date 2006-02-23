/*
 * ShowHelpUrlInBrowser.java
 *
 * Created on October 12, 2004, 6:38 PM
 */

package cern.laser.guiplatform.actions.alarms;

import java.net.URL;

import org.apache.log4j.Logger;
import org.openide.nodes.Node;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;
import org.openide.util.actions.NodeAction;

import cern.laser.guiplatform.alarms.AlarmBean;
import cern.laser.guiplatform.alarms.AlarmBeanNode;
import cern.laser.guiplatform.util.BrowserLauncher;
import cern.laser.guiplatform.util.LogFactory;

/**
 * This action runs default browser in a OS with help URL as a parameter
 *
 * @author  woloszyn
 */
public class ShowHelpUrlInBrowserAction extends NodeAction {
    
    /** action name */
    private static final String name =
    NbBundle.getMessage(ShowHelpUrlInBrowserAction.class, "LBL_Action_ShowHelpUrlInBrowser_action_name");
    
    /** logger */
    private static final Logger logger =
    LogFactory.getLogger(ShowHelpUrlInBrowserAction.class.getName());
    
    protected boolean enable(Node[] node) {
        boolean result = true;
        
        for (int i = 0; i < node.length; i++) {
            try {
                AlarmBean alarm = (AlarmBean) ((AlarmBeanNode)node[i]).getBean();
                if ( alarm.getCommentedAlarm().getAlarm().getHelpURL() == null) {
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
        return "cern/laser/guiplatform/images/url.gif";
    }
    
    public org.openide.util.HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
    }
    
    protected void performAction(Node [] activatedNodes) {
        try {
            for (int i = 0; i < activatedNodes.length; i++) {
                URL url = ((AlarmBean) ((AlarmBeanNode)activatedNodes[i]).getBean()).getCommentedAlarm().getAlarm().getHelpURL();
                BrowserLauncher.openURL(url.toExternalForm());
            }
        }
        catch ( Exception e) {
            logger.debug(e.getMessage());
        }
        
    }
}

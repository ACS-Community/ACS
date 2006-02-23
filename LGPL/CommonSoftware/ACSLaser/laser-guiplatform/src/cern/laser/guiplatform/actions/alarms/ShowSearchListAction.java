package cern.laser.guiplatform.actions.alarms;

import org.apache.log4j.Logger;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;
import org.openide.util.actions.CallableSystemAction;

import cern.laser.guiplatform.util.LogFactory;

/** Action that can always be invoked and work procedurally.
 *
 * @author bartek
 */
public class ShowSearchListAction extends CallableSystemAction {
    
    /** logger */
    private Logger logger = LogFactory.getLogger(ShowSearchListAction.class.getName());
    
    
    public void performAction() {
        // do what you want
    }
    
    public String getName() {
        return NbBundle.getMessage(ShowSearchListAction.class, 
            "LBL_Action_ShowSearchListAction_action_name");
    }
    
    protected String iconResource() {
        return null;
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(ShowSearchListAction.class);
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(ShowSearchListAction.class, "HINT_Action"));
     * }
     */
    public boolean isEnabled() {
        return false;        
    }
    
}

package cern.laser.guiplatform.actions.alarms;

import org.apache.log4j.Logger;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

import cern.laser.guiplatform.actions.support.CallableSystemAction;
import cern.laser.guiplatform.alarms.AlarmContainer;
import cern.laser.guiplatform.util.LogFactory;
//import org.openide.util.actions.CallableSystemAction;

/** Action that can always be invoked and work procedurally.
 *
 * @author pawlowsk
 */
public class ClearInhibitListAction extends CallableSystemAction {
    
    /** logger */
    private static Logger logger = 
        LogFactory.getLogger(ClearInhibitListAction.class.getName());

     public void performAction() {
        // do what you want
        AlarmContainer.getDefault().clearInhibitList();
        // TODO: TraceLogger.log(user remove new idicators);
    }
    
    public String getName() {
        return NbBundle.getMessage(ClearInhibitListAction.class, 
            "LBL_Action_ClearInhibitListAction_action_name");
    }
    
    protected String iconResource() {
        return null;
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(ClearInhibitListAction.class);
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(ClearInhibitListAction.class, "HINT_Action"));
     * }
     */
    
}

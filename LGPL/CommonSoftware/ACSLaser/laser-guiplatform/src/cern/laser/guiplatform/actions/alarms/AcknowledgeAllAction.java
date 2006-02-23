package cern.laser.guiplatform.actions.alarms;

import org.apache.log4j.Logger;
import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;
import org.openide.util.actions.CallableSystemAction;

import cern.laser.guiplatform.util.LogFactory;

/** Action that can always be invoked and work procedurally.
 *
 * @author bartek
 */
public class AcknowledgeAllAction extends CallableSystemAction {

    /** logger */
    private Logger logger = LogFactory.getLogger(AcknowledgeAllAction.class.getName());
    
    
    public void performAction() {
        // do what you want
        
        NotifyDescriptor desc = new NotifyDescriptor.Message("This is not " +
            "implemented yet");
        DialogDisplayer.getDefault().notify(desc);
        
    }
    
    public String getName() {
        return NbBundle.getMessage(AcknowledgeAllAction.class, 
                                   "LBL_Action_AcknowledgeAllAction_action_name");
    }
    
    protected String iconResource() {
        return "org/openide/resources/actions/buildAll.gif";
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(AcknowledgeAllActionAction.class);
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(AcknowledgeAllActionAction.class, "HINT_Action"));
     * }
     */
    
}

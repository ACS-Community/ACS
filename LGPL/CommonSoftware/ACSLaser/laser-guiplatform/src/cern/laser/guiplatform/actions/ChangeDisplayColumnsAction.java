package cern.laser.guiplatform.actions;

import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;
import org.openide.util.actions.CallbackSystemAction;

/** Action that can always be invoked and work procedurally.
 *
 * @author pawlowsk
 */
public class ChangeDisplayColumnsAction extends CallbackSystemAction {
    
    public void performAction() {
        // do what you want  
    }
    
    public String getName() {
        return NbBundle.getMessage(ChangeDisplayColumnsAction.class, 
                                    "LBL_ChangeDisplayColumnsAction");
    }
    
    protected String iconResource() {
        return "cern/laser/guiplatform/images/changeDisplayColumns.gif";
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(ChangeDisplayColumnsAction.class);
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(ChangeDisplayColumnsAction.class, "HINT_Action"));
     * }
     */
    
}

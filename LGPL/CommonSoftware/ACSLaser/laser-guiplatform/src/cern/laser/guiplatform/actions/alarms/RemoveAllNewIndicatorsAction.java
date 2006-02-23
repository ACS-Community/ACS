package cern.laser.guiplatform.actions.alarms;

import javax.swing.Action;

import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

import cern.laser.guiplatform.actions.support.CallableSystemAction;
import cern.laser.guiplatform.alarms.AlarmContainer;
//import org.openide.util.actions.CallableSystemAction;

/** Action that can always be invoked and work procedurally.
 * This action remove all new alarms from active list
 *
 * @author pawlowsk
 */
public class RemoveAllNewIndicatorsAction extends CallableSystemAction {
    
    public void performAction() {
        // do what you want
        
        //AlarmContainer.getDefault().removeNewOrTerminatedAlarms("NEW");
        AlarmContainer.getDefault().makeAlarmNotNew();
        // TODO: TraceLogger.log(user remove new idicators);


        
    }
    
    public String getName() {
        return NbBundle.getMessage(RemoveAllNewIndicatorsAction.class,
                                "LBL_Action_RemoveAllNewIndicators_action_name");
    }
    
    protected String iconResource() {
        return "org/openide/resources/actions/delete.gif";
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(RemoveAllNewIndicatorsAction.class);
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     */
     protected void initialize() {
        super.initialize();
        putProperty(Action.SHORT_DESCRIPTION, 
                    NbBundle.getMessage(RemoveAllNewIndicatorsAction.class, 
                    "HINT_RemoveAllNewIndicatorsAction"));
     }
}

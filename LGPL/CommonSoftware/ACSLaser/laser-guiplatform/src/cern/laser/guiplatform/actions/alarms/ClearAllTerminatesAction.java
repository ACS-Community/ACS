package cern.laser.guiplatform.actions.alarms;

import org.apache.log4j.Logger;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

import cern.laser.guiplatform.actions.support.CallableSystemAction;
import cern.laser.guiplatform.alarms.AlarmContainer;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;
import cern.laser.guiplatform.windows.ActiveListExplorerPanel;
//import org.openide.util.actions.CallableSystemAction;

/** Action that can always be invoked and work procedurally.
 *
 * @author bartek
 */
public class ClearAllTerminatesAction extends CallableSystemAction {
    
    /** logger */
    private Logger logger = 
        LogFactory.getLogger(ClearAllTerminatesAction.class.getName());
    

    
    
    public void performAction() {
        // do what you want
        AlarmContainer.getDefault().removeNewOrTerminatedAlarms("TERMINATE");

        AcWindowManager.setZeroSelectedNodes(
                org.openide.util.NbBundle.getMessage(
                                    ActiveListExplorerPanel.class, 
                                   "LBL_Active_list_component_name")); 

        //NotifyDescriptor desc = new NotifyDescriptor.Message("This is not " +
        //    "implemented yet");
        //DialogDisplayer.getDefault().notify(desc);

    }
    
    public String getName() {
        return NbBundle.getMessage(ClearAllTerminatesAction.class, 
                                "LBL_Action_ClearAllTerminatesAction_action_name");
    }
    
    protected String iconResource() {
        return "org/openide/resources/actions/cleanAll.gif";
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(ClearAllTerminatesAction.class);
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(ClearAllTerminatesAction.class, "HINT_Action"));
     * }
     */
    
}

package cern.laser.guiplatform.actions.alarms;

import org.apache.log4j.Logger;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

import cern.laser.guiplatform.actions.support.CallableSystemAction;
import cern.laser.guiplatform.alarms.AlarmContainer;
import cern.laser.guiplatform.alarms.AlarmNodeManager;
import cern.laser.guiplatform.util.Constants;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;
import cern.laser.guiplatform.windows.ActiveListExplorerPanel;
//import org.openide.util.actions.CallableSystemAction;


/** Action that can always be invoked and work procedurally.
 *
 * @author bartek
 */
public class ShowInhibitListAction extends CallableSystemAction {
    
    /** logger */
    private Logger logger = LogFactory.getLogger(ShowInhibitListAction.class.getName());

    /** inhibit list component name */
    private String inhibitListCompName = 
        NbBundle.getMessage(cern.laser.guiplatform.windows.ActiveListExplorerPanel.class, 
                            "LBL_Inhibit_list_component_name");
 
        
    public void performAction() {
        // do what you want
        AcWindowManager.setStatusText("Show inhibit list action is running .......");
         AlarmNodeManager inhibitListManager = 
            AlarmContainer.getDefault().getAlarmNodeManager(
                                        Constants.INHIBIT_LISTENER_KEY);

        //String [] columnToDisplay = AppRegister.getInstance().getBehaviour().getColumnsToDisplay();  
        String [] columnToDisplay = Constants.getColumnsToDisplay();  
        ActiveListExplorerPanel inhibitList = 
            new ActiveListExplorerPanel(inhibitListManager, inhibitListCompName, columnToDisplay);

        inhibitList.open();
        AcWindowManager.setStatusText("Show inhibit list action finished");
 
    }
    
    public String getName() {
        return NbBundle.getMessage(ShowInhibitListAction.class, 
                                "LBL_Action_ShowInhibitListAction_action_name");
    }
    
    protected String iconResource() {
        return null;
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(ShowInhibitListAction.class);
    }

    public boolean isEnabled() {

        return super.isEnabled() && !AcWindowManager.isOpened(inhibitListCompName);
    }    

    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(ShowInhibitListAction.class, "HINT_Action"));
     * }
     */


    
}

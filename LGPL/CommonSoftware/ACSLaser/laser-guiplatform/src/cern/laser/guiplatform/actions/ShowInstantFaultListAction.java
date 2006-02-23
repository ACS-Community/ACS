package cern.laser.guiplatform.actions;

import org.apache.log4j.Logger;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;

import cern.laser.guiplatform.actions.support.CallableSystemAction;
import cern.laser.guiplatform.alarms.AlarmContainer;
import cern.laser.guiplatform.alarms.AlarmNodeManager;
import cern.laser.guiplatform.util.AppRegister;
import cern.laser.guiplatform.util.Constants;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;
import cern.laser.guiplatform.windows.InstantFaultExplorer;

/** Action that can always be invoked and work procedurally.
 *
 * @author pawlowsk
 */
public class ShowInstantFaultListAction extends CallableSystemAction {
    
    /** logger */
    private static Logger logger = 
        LogFactory.getLogger(ShowInstantFaultListAction.class.getName());
    
    private String instantListCompName = NbBundle.getMessage(
            cern.laser.guiplatform.windows.DisplayableColumnExplorer.class, // this should be changed to another class
            "LBL_Instant_list_component_name"
            );
    
    public void performAction() {
        // do what you want
        AlarmNodeManager instantListManager =
            AlarmContainer.getDefault().getAlarmNodeManager(
                                                Constants.INSTANT_LISTENER_KEY);


        String[] columnToDisplay = 
            AppRegister.getInstance().getBehaviour().getColumnsToDisplay();
        TopComponent top = new InstantFaultExplorer(instantListManager, columnToDisplay);
        AcWindowManager.openInMode(Constants.INSTANT_LIST_MODE_NAME, top);
        
        //TopComponent top = WindowUtils.findTopComponent(AcWindowManager.findWorkspace(),
        //                                                instantListCompName);
        //top.setVisible(true);
    }
    
    public String getName() {
        return NbBundle.getMessage(ShowInstantFaultListAction.class, 
                                    "LBL_ShowInstantFaultListAction");
    }
    
    protected String iconResource() {
        return "org/openide/resources/actions/undock.gif";
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(ShowInstantFaultListAction.class);
    }
    
    public boolean isEnabled() {
        // check if Instant list topComponent is opened 
        // if so should be disabled        
        //logger.debug("isEnabled " + (super.isEnabled() && !AcWindowManager.isOpened(instantListCompName)));
        return super.isEnabled() && !AcWindowManager.isOpened(instantListCompName);
        //return super.isEnabled();// && !WindowUtils.findTopComponent(instantListCompName).isVisible();
                    
    }
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(ShowInstantFaultListAction.class, "HINT_Action"));
     * }
     */
    
}

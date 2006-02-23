package cern.laser.guiplatform.actions;

import org.apache.log4j.Logger;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

import cern.laser.guiplatform.actions.support.CallableSystemAction;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;

/** Action that can always be invoked and work procedurally.
 * This action shows instant fault state list
 * @author pawlowsk
 */
public class HideInstantFaultListAction extends CallableSystemAction {
    
    /** logger */
    private static Logger logger = 
        LogFactory.getLogger(HideInstantFaultListAction.class.getName());
    
    private String instantListCompName = NbBundle.getMessage(
            cern.laser.guiplatform.windows.DisplayableColumnExplorer.class, // this should be changed to another class
            "LBL_Instant_list_component_name"
            );
    
    
    public void performAction() {
        // do what you want
        AcWindowManager.closeComponent(instantListCompName);
        //TopComponent top = WindowUtils.findTopComponent(AcWindowManager.findWorkspace(),
        //                                                instantListCompName);
        //top.setVisible(false);
    }
    
    public String getName() {
        return NbBundle.getMessage(HideInstantFaultListAction.class, "LBL_HideInstantFaultListAction");
    }
    
    protected String iconResource() {
        return "cern/laser/guiplatform/images/dock.gif";
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(ShowInstantFaultListAction.class);
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(ShowInstantFaultListAction.class, "HINT_Action"));
     * }
     */
    public boolean isEnabled() {
        return super.isEnabled() && AcWindowManager.isOpened(instantListCompName);
        
    }
}

package cern.laser.guiplatform.actions.alarms;

import org.apache.log4j.Logger;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

import cern.laser.guiplatform.actions.support.CallableSystemAction;
import cern.laser.guiplatform.alarms.AlarmContainer;
import cern.laser.guiplatform.util.LogFactory;
//import org.openide.util.actions.CallableSystemAction;

/** Action that can always be invoked and work procedurally.
 * This action clear autoHighlidhted list
 * 
 * To clear autKlaxon list use ClearAutoKlaxonList action
 *
 * @author pawlowsk
 */
public class ClearHighlightedAndKlaxonListAction extends CallableSystemAction {

    /** logger */
    private static Logger logger = 
        LogFactory.getLogger(ClearHighlightedAndKlaxonListAction.class.getName());
   
    public void performAction() {
        // do what you want
        AlarmContainer.getDefault().clearAutoHighlightedList();
        // TODO: TraceLogger.log(user cleared highlighted list);
    }
    
    public String getName() {
        return NbBundle.getMessage(ClearHighlightedAndKlaxonListAction.class, 
        "LBL_Action_ClearHighlightedAndKlaxonListAction_action_name");
    }
    
    protected String iconResource() {
        return "cern/laser/guiplatform/images/defaultActionIcon.gif";
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(ClearHighlightedAndKlaxonListAction.class);
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(ClearHighlightedAndKlaxonListAction.class, "HINT_Action"));
     * }
     */
    
}

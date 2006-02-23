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
public class ShowHighlightedAndKlaxonListAction extends CallableSystemAction {

    /** logger */
    private Logger logger = 
        LogFactory.getLogger(ShowHighlightedAndKlaxonListAction.class.getName());
   
    /** mask list component name */
    private String maskListCompName = 
        NbBundle.getMessage(cern.laser.guiplatform.windows.ActiveListExplorerPanel.class, 
                            "LBL_HighligtedAndKlaxon_list_component_name");
  
    public void performAction() {
        // do what you want
        AlarmNodeManager highlightedAndKlaxonListManager = 
            AlarmContainer.getDefault().getAlarmNodeManager(
                                        Constants.HIGHLITED_LISTENER_KEY);
        //String [] columnToDisplay = AppRegister.getInstance().getBehaviour().getColumnsToDisplay();  
        String [] columnToDisplay = Constants.getColumnsToDisplay();  
        ActiveListExplorerPanel highlightedList = 
            new ActiveListExplorerPanel(highlightedAndKlaxonListManager, 
                                        maskListCompName, columnToDisplay);

        highlightedList.open();
 
    }
    
    public String getName() {
        return NbBundle.getMessage(ShowHighlightedAndKlaxonListAction.class, 
            "LBL_Action_ShowHighlightedAndKlaxonListAction_action_name");
    }
    
    protected String iconResource() {
        return null;
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(ShowHighlightedAndKlaxonListAction.class);
    }

    public boolean isEnabled() {

        return super.isEnabled() && !AcWindowManager.isOpened(maskListCompName);
    }    

    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(ShowHighlightedAndKlaxonListAction.class, "HINT_Action"));
     * }
     */
    
}

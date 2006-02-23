package cern.laser.guiplatform.actions.alarms;

import org.apache.log4j.Logger;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;
import org.openide.util.actions.CallableSystemAction;

import cern.laser.console.Behaviour;
import cern.laser.guiplatform.alarms.AlarmContainer;
import cern.laser.guiplatform.util.AppRegister;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;

/** Action that can always be invoked and work procedurally.
 *
 * @author bartek
 */
public class ChangeFontAction extends CallableSystemAction {
    
    /** logger */
    private Logger logger = LogFactory.getLogger(ChangeFontAction.class.getName());
    
    
    public void performAction() {
        if ( AppRegister.getInstance().isFontBig() == true ) {
            AppRegister.getInstance().setFontBig(false);
        }
        else {
            AppRegister.getInstance().setFontBig(true);
        }
        AlarmContainer.getDefault().fireFontChanged();
        String activeListTopName = NbBundle.getMessage(
                                 cern.laser.guiplatform.windows.ActiveListExplorerPanel.class, 
                                 "LBL_Active_list_component_name");        
        Behaviour behaviour = AppRegister.getInstance().getBehaviour();
        AcWindowManager.setTableColumns(activeListTopName , behaviour.getColumnsToDisplay()); 
        
        // storing default font size in BL
        
        AlarmContainer.getDefault().setActiveListFont( AppRegister.getInstance().isFontBig() );
    }
    
    public String getName() {
        return NbBundle.getMessage(ChangeFontAction.class, 
                                    "LBL_Action_ChangeFont_action_name");
    }
    
    protected String iconResource() {
        //return "cern/laser/guiplatform/actions/alarms/ChangeFontActionIcon.gif";
        
        // font from linux menu (font)
        return null;
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(ChangeFontAction.class);
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(ChangeFontAction.class, "HINT_Action"));
     * }
     */
    
}

package cern.laser.guiplatform.actions;

import org.apache.log4j.Logger;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

import cern.laser.console.User;
import cern.laser.guiplatform.actions.support.CallableSystemAction;
import cern.laser.guiplatform.printing.DefaultPrinterDialog;
import cern.laser.guiplatform.util.AppRegister;
import cern.laser.guiplatform.util.LogFactory;

//import org.openide.util.actions.CallbackSystemAction;
/** Action that can always be invoked and work procedurally.
 *
 * @author pawlowsk
 */
public class SetPrinterAction extends CallableSystemAction {
//public class SetPrinterAction extends CallbackSystemAction {    
    
    static final Logger logger = LogFactory.getLogger(SetPrinterAction.class.getName());
    
    public void performAction() { 
        new DefaultPrinterDialog(null, true).show();
        logger.debug(" SetPrinterAction.performAction() ");
        
    }
    
    public String getName() {
        return NbBundle.getMessage(SetPrinterAction.class, "LBL_SetPrinterAction");
    }
    
    protected String iconResource() {
        //return "org/openide/resources/actions/pageSetup.gif";
        return "org/openide/resources/printSettings.gif";
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(PrinterSetupAction.class);
    }
    
    public boolean isEnabled() {
    	User user = AppRegister.getInstance().getRegisteredUser();
        return super.isEnabled() && user != null;
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(PrinterSetupAction.class, "HINT_Action"));
     * }
     */

    public Object getActionMapKey() {
        return "TEST";
    }
    
    
    
}


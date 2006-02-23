package cern.laser.guiplatform.actions;

import org.apache.log4j.Logger;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

import cern.laser.guiplatform.actions.support.CallableSystemAction;
import cern.laser.guiplatform.printing.PrintUtil;
import cern.laser.guiplatform.printing.PrintUtil.MissingPrintableObjectException;
import cern.laser.guiplatform.printing.PrintUtil.SettingsNotCorrectException;
import cern.laser.guiplatform.util.AppRegister;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;
import cern.laser.guiplatform.windows.ActiveListExplorerPanel;

/** Action that can always be invoked and work procedurally.
 *
 * @author pawlowsk
 */
public class PrintActiveListAction extends CallableSystemAction {

    private static Logger logger = 
        LogFactory.getLogger(PrintActiveListAction.class.getName());
    
    public void performAction() {
        PrintUtil pu = new PrintUtil();
        ActiveListExplorerPanel panel = AppRegister.getInstance().getRegisteredActiveListExplorerPanel();
        pu.setPrintable( panel.getView() );
        try {
            if ( pu.printDialog() ){
                AcWindowManager.setStatusText("Print action is running .......");
                pu.print();
                AcWindowManager.setStatusText("Print action is finished .......");
            }
        }
        catch( MissingPrintableObjectException mpoe ){
            mpoe.printStackTrace();
        }
        catch ( SettingsNotCorrectException snce) {
            snce.printStackTrace();
        }        
    }
    
    public String getName() {
        return NbBundle.getMessage(PrintActiveListAction.class, "LBL_PrintActiveListAction");
    }
    
    protected String iconResource() {
        return "org/openide/resources/actions/print.gif";
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(PrintActiveListActionAction.class);
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(PrintActiveListActionAction.class, "HINT_Action"));
     * }
     */
}

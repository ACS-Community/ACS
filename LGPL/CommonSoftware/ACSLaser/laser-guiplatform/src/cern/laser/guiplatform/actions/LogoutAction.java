package cern.laser.guiplatform.actions;

import org.apache.log4j.Logger;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

import alma.acs.container.ContainerServicesBase;

import cern.laser.client.LaserException;
import cern.laser.client.services.selection.AlarmSelectionHandler;
import cern.laser.console.User;
import cern.laser.guiplatform.actions.support.CallableSystemAction;
import cern.laser.guiplatform.alarms.AlarmContainer;
import cern.laser.guiplatform.alarms.AlarmSelectionHandlerFactory;
import cern.laser.guiplatform.logging.TraceLogger;
import cern.laser.guiplatform.util.AppRegister;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;

/** Action that can always be invoked and work procedurally.
 *
 * @author pawlowsk
 */
public class LogoutAction extends CallableSystemAction {

    private Logger logger = LogFactory.getLogger(LogoutAction.class.getName());
    
    private ContainerServicesBase contSvcs;
    
    public LogoutAction(ContainerServicesBase contSvcs) {
    	this.contSvcs=contSvcs;
    }

    public void performAction() {
        // do what you want
        User user = AppRegister.getInstance().getRegisteredUser();
        try {
            // here user should always be registered
            logger.debug("user " + user.getName() + " has just logged out");
            TraceLogger.log("user " + user.getName() + " has just logged out");
        } catch (LaserException le) {
            logger.error(le.getRootCause());
            TraceLogger.log("User.getName() " + le.getRootCause());
        }
            
        // close all apropriate TopComponents
        AcWindowManager.closeAllTopComponents();

        try {
            AlarmSelectionHandler jms_selectionHandler = 
            AlarmSelectionHandlerFactory.getHandler(contSvcs);
            // disconnect from BL
            jms_selectionHandler.resetSelection();
        } catch (LaserException le) {
            logger.error(le, le.fillInStackTrace());
            logger.error(le.getRootCause(), le.getRootCause().fillInStackTrace());
        }

        cern.laser.guiplatform.util.ProxyBuffer.getDefault().disable();
        cern.laser.guiplatform.util.ProxyBuffer.getDefault().close();


        // clear regiester
        AppRegister.getInstance().cleanRegister();
       
        // clear AlarmContainer
        AlarmContainer.getDefault().clearContainer();
        AppRegister.getInstance().unregisterUser();
        
    }
    
    public String getName() {
        return NbBundle.getMessage(LogoutAction.class, "LBL_LogoutAction");
    }
    
    protected String iconResource() {
        return "cern/laser/guiplatform/images/exit.gif";
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(LogoutAction.class);
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(LogoutAction.class, "HINT_Action"));
     * }
     */
    
}

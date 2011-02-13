package cern.laser.guiplatform.actions;

import org.apache.log4j.Logger;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

import alma.acs.container.ContainerServicesBase;

import cern.laser.guiplatform.actions.support.CallableSystemAction;
import cern.laser.guiplatform.util.Constants;
import cern.laser.guiplatform.util.LogFactory;
import cern.laser.guiplatform.windowmanager.AcWindowManager;
import cern.laser.guiplatform.windows.login.AlarmConsoleLoginWindow;

/** Action that can always be invoked and work procedurally.
 * This is login action
 *
 * @author pawlowsk
 */
public class LoginAction extends CallableSystemAction {
    
    private Logger logger = LogFactory.getLogger(LoginAction.class.getName());
    
    private final ContainerServicesBase contSvcs;
    
    public LoginAction(ContainerServicesBase contSvcs ) {
    	this.contSvcs=contSvcs;
    }
    
    public void performAction() {
        // do what you want
        
        AcWindowManager.openInMode(Constants.ALARM_LOGIN_MODE_NAME, 
                                    new AlarmConsoleLoginWindow(contSvcs));

        //WindowUtils.openInMode(new AlarmConsoleLoginWindow(),
        //                         Constants.ALARM_LOGIN_MODE_NAME, WindowUtils.DESKTOP_FRAME);
        
    }
    
    public String getName() {
        return NbBundle.getMessage(LoginAction.class, "LBL_LoginAction");
    }
    
    protected String iconResource() {
        return "cern/laser/guiplatform/images/lock.gif";
    }
    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx(LoginActionAction.class);
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize() {
     * super.initialize();
     * putProperty(Action.SHORT_DESCRIPTION, NbBundle.getMessage(LoginActionAction.class, "HINT_Action"));
     * }
     */
    
    public boolean isEnabled() {
        //logger.debug("isEnabled() " + !super.isEnabled());
        return !super.isEnabled();        
    }
}

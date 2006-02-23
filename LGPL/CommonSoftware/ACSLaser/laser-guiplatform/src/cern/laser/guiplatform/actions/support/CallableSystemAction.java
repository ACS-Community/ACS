/*
 * CallableSystemAction.java
 *
 * Created on 10 maj 2003, 16:21
 */

package cern.laser.guiplatform.actions.support;

import org.apache.log4j.Logger;
import org.openide.util.HelpCtx;

import cern.laser.guiplatform.util.AppRegister;
import cern.laser.guiplatform.util.LogFactory;

/**
 * This action is enabled only when user is logged. All CallableSystemActions
 * in this project should extend this action.
 *
 * @author  bartek
 */
public /*abstract*/ class CallableSystemAction 
    extends org.openide.util.actions.CallableSystemAction {
    
    private static Logger logger = LogFactory.getLogger(CallableSystemAction.class.getName());

    protected static boolean isEnabled = false;

    
    public boolean isEnabled() {        
        //logger.debug(" isEnabled ->" + (AppRegister.getInstance().getRegisteredUser() != null));
        return super.isEnabled() && 
                AppRegister.getInstance().getRegisteredUser() != null;
    }
    
   
    public void performAction() { }
    public org.openide.util.HelpCtx getHelpCtx() {
       return HelpCtx.DEFAULT_HELP;
    }

    //public boolean isEnabled() {
    //   return isEnabled;
    //}

    //public void setEnabled(boolean enabled) {
    //   isEnabled = enabled;
    //} 
    
    public String getName() {
        return "";
    }
    
}

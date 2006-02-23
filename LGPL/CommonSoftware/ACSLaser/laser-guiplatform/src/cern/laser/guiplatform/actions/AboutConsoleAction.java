package cern.laser.guiplatform.actions;

//import cern.gp.actions.support.CallableSystemAction;
import org.openide.NotifyDescriptor;
import org.openide.util.HelpCtx;
import org.openide.util.actions.CallableSystemAction;

import cern.gp.util.GPManager;

/** Action that displays pop up window with application info.
 *
 * @author  pawlowsk
 */
public class AboutConsoleAction extends CallableSystemAction {
    
    /**
     * displays an info notification window
     */
    public void performAction() {
        NotifyDescriptor desc;
        desc = new NotifyDescriptor.Message(
            "Alarm Console Prototype v 0.7.0\nLaser Project \nModified on the 28/08/2004\n" +
            "If you have any problems,\nsend email to Piotr.Woloszyn@cern.ch",
            NotifyDescriptor.INFORMATION_MESSAGE);
        GPManager.notify(desc);        
    }
    
    /**
     * @return "About Alarm Console"
     */    
    public String getName() {
        return "About Alarm Console";
    }
    
    /** 
     * @return question mark
     */    
    protected String iconResource() {
        //return "cern/laser/guiplatform/images/question.gif";
        return "org/netbeans/core/resources/actions/about.gif";
    }
    
    /**
     * 
     * @return DEFAULT HELP 
     */    
    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
        // If you will provide context help then use:
        // return new HelpCtx (AboutConsoleAction.class);
    }
    
    /** Perform extra initialization of this action's singleton.
     * PLEASE do not use constructors for this purpose!
     * protected void initialize () {
     * super.initialize ();
     * putProperty (Action.SHORT_DESCRIPTION, NbBundle.getMessage (AboutConsoleAction.class, "HINT_Action"));
     * }
     */
}

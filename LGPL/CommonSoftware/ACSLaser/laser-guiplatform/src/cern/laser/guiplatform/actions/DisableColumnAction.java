/*
 * DisableColumnAction.java
 *
 * Created on April 15, 2003, 1:27 PM
 */

package cern.laser.guiplatform.actions;

import org.apache.log4j.Logger;
import org.openide.util.NbBundle;

import cern.gp.actions.support.BeanActionSupport;
import cern.laser.guiplatform.beans.DisplayColumnBean;
import cern.laser.guiplatform.capabilities.DisableCapability;
import cern.laser.guiplatform.util.LogFactory;

/**
 * This class disables column, this column should be deleted from alarm
 * screen
 *
 * @author  pawlowsk
 */
public class DisableColumnAction extends BeanActionSupport {

    static Logger logger = 
        LogFactory.getLogger(DisableColumnAction.class.getName());    


    /** Creates a new instance of DisableColumnAction */
    public DisableColumnAction() {
        super(DisableCapability.class);
    }
    /* 
    protected void performAction(cern.gp.nodes.GPNode[] gPNode) {
    }
    
    public org.openide.util.HelpCtx getHelpCtx() {
        return new HelpCtx(DisableColumnAction.class);
    }
    */ 
    
    protected boolean enable(org.openide.nodes.Node[] node) {
        boolean result = false;
        org.openide.nodes.Node.Cookie cookie = null;//
        if ( node.length > 0 ) {
            cookie = node[0].getCookie(DisplayColumnBean.class);
            if ( cookie != null )
                result = ((DisplayColumnBean) cookie).isEnabled();
        }
        return super.enable(node) && result;
        
    }
  
    
    
    protected String iconResource() {
        return "org/openide/resources/actions/clean.gif";
    }
    
    public String getName() {
        return NbBundle.getMessage(DisableColumnAction.class,
                "DISABLE_COLUMN_action_name");
    }
    
    /**
     * this is only used if we connect JButtons to this action. it makes sure the JButtons are
     * refreshed properly when there is a focus change.
     */
    protected boolean surviveFocusChange() { return true; }  
}

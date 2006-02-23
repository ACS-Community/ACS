/*
 * EnableColumnAction.java
 *
 * Created on April 16, 2003, 10:10 AM
 */

package cern.laser.guiplatform.actions;

import org.apache.log4j.Logger;
import org.openide.util.NbBundle;

import cern.laser.guiplatform.beans.DisplayColumnBean;
import cern.laser.guiplatform.capabilities.EnableCapability;
import cern.laser.guiplatform.util.LogFactory;

/**
 *
 * @author  pawlowsk
 */
public class EnableColumnAction extends cern.gp.actions.support.BeanActionSupport {

    static Logger logger = 
        LogFactory.getLogger(EnableColumnAction.class.getName());    

    
    /** Creates a new instance of EnableColumnAction */
    public EnableColumnAction() {
        super(EnableCapability.class);
    }
    
    public String getName() {
        return NbBundle.getMessage(DisableColumnAction.class,
                "ENABLE_COLUMN_action_name");
    }
    
    protected String iconResource() {
        return "org/openide/resources/actions/build.gif";
    }
    
    protected boolean surviveFocusChange() {
        return true;
    }
    
    
    protected boolean enable(org.openide.nodes.Node[] node) {
        boolean result = false;
        org.openide.nodes.Node.Cookie cookie = null;//
        if ( node.length > 0 ) {
            cookie = node[0].getCookie(DisplayColumnBean.class);
            if ( cookie != null )
                result = !((DisplayColumnBean) cookie).isEnabled();
        }
        return super.enable(node) && result;
        
    }  
}

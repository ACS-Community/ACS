/*
 * MoveUpAction.java
 *
 * Created on October 14, 2003, 9:57 AM
 */

package cern.laser.guiplatform.actions;

import cern.laser.guiplatform.util.AppRegister;

/**
 * This action is created in order to be able to change Save button status
 * (ConfiguraionPanel)
 * 
 * @author  Bartlomiej Pawlowski <Bartlomiej.Pawlowski@cern.ch>
 */
public class MoveUpConfChangeEventAction extends cern.gp.actions.MoveUpAction {
    
    
    protected void performAction(org.openide.nodes.Node[] node) {
        // change save button status
        AppRegister.getInstance().notifyConfigurationChange();
        super.performAction(node);
    }
    
    
}

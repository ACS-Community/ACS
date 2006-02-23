/*
 * ActionUtils.java
 *
 * Created on August 26, 2003, 12:00 PM
 */

package cern.laser.guiplatform.util.actions;

import javax.swing.Action;

import org.openide.util.ContextAwareAction;
import org.openide.windows.TopComponent;

/**
 *
 * @author  Bartlomiej Pawlowski <Bartlomiej.Pawlowski@cern.ch>
 */
public class ActionUtils {
    
    /** Creates a new instance of ActionUtils */
    //private ActionUtils() {
    //}
    
    /**
     * new way of creating action, when Explorer is placed on 
     * another TopComponent
     *  
     *
     * @param comp TopComponent for which action should be created
     * @param action action to be created
     */
    public static Action createActionForComp(TopComponent comp, Action action) {
        return ((ContextAwareAction)action).createContextAwareInstance(comp.getLookup());
    }
    
}

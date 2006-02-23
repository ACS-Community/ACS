/*
 * AlarmColoredEditor.java
 *
 * Created on 27 maj 2003, 22:56
 */

package cern.laser.guiplatform.alarms.editors;

import java.awt.Graphics;
import java.awt.Rectangle;
import java.beans.PropertyEditorSupport;

import cern.gp.beans.editors.support.BeanDependentPropertyEditor;
import cern.gp.beans.editors.support.ColorMaster;
import cern.gp.beans.editors.support.ColoredEditorHelper;

/**
 *
 * @author  bartek
 */
//public class AlarmColoredEditor extends cern.gp.beans.editors.support.ColoredEditorSupport implements cern.gp.beans.editors.support.BeanDependentPropertyEditor {
public class AlarmColoredEditor extends PropertyEditorSupport 
    implements BeanDependentPropertyEditor {    
        
    /** color helper */
    private ColoredEditorHelper colorHelper = null;
    
    
    public void initializePropertyEditor(Object obj, String str) {
        if ( obj instanceof ColorMaster )
            colorHelper = new ColoredEditorHelper(this, (ColorMaster) obj);
    }
    
    /** Determines whether the class will honor the painValue method.
     *
     * @return  True if the class will honor the paintValue method.
     *
     */
    public boolean isPaintable() {
        return colorHelper.isPaintable();
    }
    
    /** Paint a representation of the value into a given area of screen
     * real estate.  Note that the propertyEditor is responsible for doing
     * its own clipping so that it fits into the given rectangle.
     * <p>
     * If the PropertyEditor doesn't honor paint requests (see isPaintable)
     * this method should be a silent noop.
     *
     * @param gfx  Graphics object to paint into.
     * @param box  Rectangle within graphics object into which we should paint.
     *
     */
    public void paintValue(Graphics gfx, Rectangle box) {
        colorHelper.paintValue(gfx, box);
    }
    
    
}

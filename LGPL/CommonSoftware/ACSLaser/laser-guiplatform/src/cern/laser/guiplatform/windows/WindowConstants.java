/*
 * WindowConstants.java
 *
 * Created on 10 maj 2003, 18:47
 */

package cern.laser.guiplatform.windows;

/**
 * This class contains only constant variables used for creating windows
 *
 * @author  bartek
 */
public final class WindowConstants {
    
    /** Creates a new instance of WindowConstants 
     * do not instantiate 
     */
    private WindowConstants() {
    }
  
    /** window should be placed in the center of the workspace*/
    public static final int CENTER = 1;
    /** */
    public static final int WEST = 2;
    /** */
    public static final int EAST = 3;
    
    
    /** topcomponent should fill up the whole workspace */
    public static final int ALL = 10;
    
    /** create mode with TopComponent size*/
    public static final int COMPONENT_SIZE = 11;
}

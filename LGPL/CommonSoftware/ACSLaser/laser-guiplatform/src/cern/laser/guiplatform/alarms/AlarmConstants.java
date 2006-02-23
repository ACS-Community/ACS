/*
 * Constants.java
 *
 * Created on 27 maj 2003, 22:22
 */

package cern.laser.guiplatform.alarms;

/**
 * This class stores constants variables for alarms
 * @author  bartek
 */
public class AlarmConstants {
   
    /** */ 
    public static final int PRIORITY_0 = 0;
    
    /** */ 
    public static final int PRIORITY_1 = 1;
    
    /** */ 
    public static final int PRIORITY_2 = 2;
    
    /** */ 
    public static final int PRIORITY_3 = 3;
    
    /** alarm should be only highlighed */
    public static final int HIGHLIGHTED_ONLY = 10;
    
    /** alarm should have klaxon only */
    public static final int KLAXON_ONLY = 20;
    
    /** alarm should be highlighted and have klaxon */
    public static final int HIGHLIGHTED_AND_KLAXON = 30;
    
    /** Creates a new instance of Constants 
     * do not instantiate 
     */
    private AlarmConstants() {
    }
    
}

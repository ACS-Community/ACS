/*
 * UninhibitCapability.java
 *
 * Created on May 23, 2003, 3:12 PM
 */

package cern.laser.guiplatform.capabilities;


/**
 * Capability an object implements so that it can be uninhibited. 
 * This capability is invoked by the corresponding Action 
 *
 * @author  pawlowsk
 */
public interface UninhibitCapability extends cern.gp.capabilities.Capability {
    
    /** Indicates to this object that it has to be inhibit */
    void uninhibit(/*GPNode node*/);
    
}

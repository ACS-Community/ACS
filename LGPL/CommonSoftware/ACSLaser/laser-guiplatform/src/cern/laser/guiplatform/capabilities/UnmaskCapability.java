/*
 * MaskCapability.java
 *
 * Created on May 26, 2003, 5:48 PM
 */

package cern.laser.guiplatform.capabilities;

/**
 * Capability an object implements so that it can be unmasked. 
 * This capability is invoked by the corresponding Action 
 *
 * @author  pawlowsk
 */
public interface UnmaskCapability extends cern.gp.capabilities.Capability {
    
    /** Indicates to this object that it has to be masked */
    public void unmask(/*GPNode node*/);
    
}

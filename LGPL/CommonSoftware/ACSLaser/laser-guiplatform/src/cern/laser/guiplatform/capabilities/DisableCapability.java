/*
 * DisableCapability.java
 *
 * Created on April 15, 2003, 2:36 PM
 */

package cern.laser.guiplatform.capabilities;

/**
 * Object, which implements this capability can be disabled
 * This capability is invoked by the corresponding aciton
 *
 * @author  pawlowsk
 */
public interface DisableCapability extends cern.gp.capabilities.Capability {
 
    /** tell the object that implements this nterfaces to be disabled */
    void disable();
}

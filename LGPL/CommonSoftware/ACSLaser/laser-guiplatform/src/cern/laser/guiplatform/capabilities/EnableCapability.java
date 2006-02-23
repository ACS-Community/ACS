/*
 * EnableCapability.java
 *
 * Created on April 16, 2003, 10:14 AM
 */

package cern.laser.guiplatform.capabilities;

/**
 * Object, which implements this capability can be enabled
 * This capability is invoked by the corresponding aciton
 *
 * @author  pawlowsk
 */
public interface EnableCapability extends cern.gp.capabilities.Capability {

    /** tell the object that implements this nterfaces to be enabled */
    void enable();
    
}

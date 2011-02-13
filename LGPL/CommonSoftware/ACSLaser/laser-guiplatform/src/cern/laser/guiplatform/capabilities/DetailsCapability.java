/*
 * DetailsCapability.java
 *
 * Created on June 25, 2003, 5:40 PM
 */

package cern.laser.guiplatform.capabilities;

import alma.acs.container.ContainerServicesBase;

/**
 * Capability an object implements so that it can show details. 
 * This capability is invoked by the corresponding Action 
 *
 * @author  pawlowsk
 */
public interface DetailsCapability extends cern.gp.capabilities.Capability {

    /** Indicates to this object that it has to show details */
    void details(/*GPNode node*/ContainerServicesBase contSvcs);
  
}

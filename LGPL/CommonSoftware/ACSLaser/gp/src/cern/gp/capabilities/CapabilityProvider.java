/*
 * $Id: CapabilityProvider.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.capabilities;

/**
 * <i><font size="-1" color="#FF0000">**Experimental : for internal use only** </font></i>
 * A class implementing this interface provides a set of capabilities.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public interface CapabilityProvider {
  
  public Capability[] getCapabilities();

}

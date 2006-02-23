/*
 * $Id: CapabilityProvider.java,v 1.1 2005/06/07 03:26:13 kzagar Exp $
 *
 * $Date: 2005/06/07 03:26:13 $
 * $Revision: 1.1 $
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.capabilities;

/**
 * <i><font size="-1" color="#FF0000">**Experimental : for internal use only** </font></i>
 * A class implementing this interface provides a set of capabilities.
 *
 * @version $Revision: 1.1 $  $Date: 2005/06/07 03:26:13 $
 * @author Lionel Mestre
 */
public interface CapabilityProvider {
  
  public Capability[] getCapabilities();

}

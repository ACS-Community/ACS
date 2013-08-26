/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.alarmsystem.source;

import java.sql.Timestamp;
import java.util.Properties;

/**
 * This interface is a copy of cern.laser.source.alarmsysteminterface.FaultState.
 * This is repeated here to avoid dependency with LASER.
 * 
 * @see cern.laser.source.alarmsysteminterface.FaultState
 * 
 * @author acaproni
 *
 */
public interface ACSFaultState {
	/** The active fault state descriptor.
	   */
	  public final static String ACTIVE = "ACTIVE";

	  /** The terminate fault state descriptor.
	   */
	  public final static String TERMINATE = "TERMINATE";

	  /** The change fault state descriptor.
	   */
	  public final static String CHANGE = "CHANGE";

	  /** The instant fault state descriptor.
	   */
	  public final static String INSTANT = "INSTANT";

	  /** Problem descriptor prefix reserved user property.
	   */
	  public final static String ASI_PREFIX_PROPERTY = "ASI_PREFIX";

	  /** Problem descriptor suffix reserved user property.
	   */
	  public final static String ASI_SUFFIX_PROPERTY = "ASI_SUFFIX";

	  /** Fault code accessor method.
	   * @param faultCode the fault code.
	   */
	  public void setCode(int faultCode);

	  /** Fault code accessor method.
	   * @return the fault code.
	   */
	  public int getCode();
	  
	  /** Fault descriptor accessor method.
	   * @param descriptor the fault descriptor.
	   */
	  public void setDescriptor(String descriptor);

	  /** Fault descriptor accessor method.
	   * @return String the fault descriptor.
	   */
	  public String getDescriptor();

	  /** Fault family accessor method.
	   * @param faultFamily the fault family.
	   */
	  public void setFamily(String faultFamily);

	  /** Fault family accessor method.
	   * @return the fault family.
	   */
	  public String getFamily();

	  /** Fault member accessor method.
	   * @param faultMember the fault member.
	   */
	  public void setMember(String faultMember);

	  /** Fault member accessor method.
	   * @return the fault member.
	   */
	  public String getMember();

	  /** User properties accessor method.
	   * @param properties the user properties.
	   */
	  public void setUserProperties(Properties properties);

	  /** User properties accessor method.
	   * @return Properties the user properties.
	   */
	  public Properties getUserProperties();

	  /** Timestamp accessor method.
	   * @param timestamp the timestamp.
	   */
	  public void setUserTimestamp(Timestamp timestamp);

	  /** Timestamp accessor method.
	   * @return long the timestamp.
	   */
	  public Timestamp getUserTimestamp();
	  
	  public boolean getActivatedByBackup();

	  public void setActivatedByBackup(boolean newActivatedByBackup);
	  
	  public boolean getTerminatedByBackup();

	  public void setTerminatedByBackup(boolean newTerminatedByBackup);

}

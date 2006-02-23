package cern.laser.source.alarmsysteminterface;

import java.sql.Timestamp;
import java.util.Properties;


/**
 * A fault state.
 * @author fracalde
 * @version 1.0
 */
public interface FaultState {
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

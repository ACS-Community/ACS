package cern.laser.source.alarmsysteminterface.impl;

import java.sql.Timestamp;
import java.util.Enumeration;
import java.util.Properties;

import cern.laser.source.alarmsysteminterface.ASIException;
import cern.laser.source.alarmsysteminterface.FaultState;


/**
 * A fault state implementation.
 * @author fracalde
 * @version 1.0
 */
public class FaultStateImpl implements FaultState {
  private Properties userProperties = new Properties();
  private String descriptor;

  // alarm specific attributes
  private String faultFamily = null;
  private String faultMember = null;
  private Timestamp userTimestamp = null;
  private int faultCode;
  private boolean activatedByBackup = false;
  private boolean terminatedByBackup = false;

  /** constructor
   */
  public FaultStateImpl() {
  }

  /** constructor
   * @param faultFamily the fault family
   * @param faultMember the fault member
   * @param faultCode the fault code
   */
  public FaultStateImpl(String faultFamily, String faultMember, int faultCode) {
    this.faultFamily = faultFamily;
    this.faultMember = faultMember;
    this.faultCode = faultCode;
  }

  /** fault code accessor method
   * @param faultCode the fault code
   */
  public void setCode(int faultCode) {
    this.faultCode = faultCode;
  }

  /** fault code accessor method
   * @return the fault code
   */
  public int getCode() {
    return faultCode;
  }

  /** the fault descriptor accessor method
   * @param descriptor the fault descriptor
   */
  public void setDescriptor(String descriptor) {
    this.descriptor = descriptor;
  }

  /** the fault descriptor accessor method
   * @return String the fault descriptor
   */
  public String getDescriptor() {
    return descriptor;
  }

  /** fault family accessor method
   * @param faultFamily the fault family
   */
  public void setFamily(String faultFamily) {
    this.faultFamily = faultFamily;
  }

  /** fault family accessor method
   * @return the fault family
   */
  public String getFamily() {
    return faultFamily;
  }

  /** fault member accessor method
   * @param faultMember the fault member
   */
  public void setMember(String faultMember) {
    this.faultMember = faultMember;
  }

  /** fault member accessor method
   * @return the fault member
   */
  public String getMember() {
    return faultMember;
  }

  /** set the user properties attached to the fault state
   * @param properties the user properties
   */
  public void setUserProperties(Properties properties) {
    userProperties = properties;
  }

  /** return the user properties attached to the fault state
   * @return Properties the user properties
   */
  public Properties getUserProperties() {
    return userProperties;
  }

  /** the user timestamp accessor method
   * @param timestamp the user timestamp
   */
  public void setUserTimestamp(Timestamp timestamp) {
    userTimestamp = timestamp;
  }

  /** the user timestamp accessor method
   * @return long the user timestamp
   */
  public Timestamp getUserTimestamp() {
    return userTimestamp;
  }

  /* (non-Javadoc)
   * @see cern.laser.source.alarmsysteminterface.FaultState#getActivatedByBackup()
   */
  public boolean getActivatedByBackup() {
    return activatedByBackup;
  }

  /* (non-Javadoc)
   * @see cern.laser.source.alarmsysteminterface.FaultState#setActivatedByBackup(java.lang.Boolean)
   */
  public void setActivatedByBackup(boolean newActivatedByBackup) {
    activatedByBackup = newActivatedByBackup;
  }

  /* (non-Javadoc)
   * @see cern.laser.source.alarmsysteminterface.FaultState#getTerminatedByBackup()
   */
  public boolean getTerminatedByBackup() {
    return terminatedByBackup;
  }

  /* (non-Javadoc)
   * @see cern.laser.source.alarmsysteminterface.FaultState#setTerminatedByBackup(java.lang.Boolean)
   */
  public void setTerminatedByBackup(boolean newTerminatedByBackup) {
    terminatedByBackup = newTerminatedByBackup;
  }

  /** Check for equality with respect to the fault state triplet ff,fm,fc.
   * @param obj the object to check against.
   * @return true iff the two objects are equal.
   */
  public boolean equals(Object obj) {
    if (!(obj instanceof FaultStateImpl)) {
      return false;
    }

    if (obj == null) {
      return false;
    }

    FaultStateImpl fs = (FaultStateImpl) obj;

    return value().equals(fs.value());
  }

  /** Return the object hash code.
   * @return The hash code.
   */
  public int hashCode() {
    return value().hashCode();
  }

  /** Return a String representation of a FaultState.
   * @return the String representation
   */
  public String toString() {
    StringBuffer text = new StringBuffer();
    text.append("FF : " + ((faultFamily != null) ? faultFamily : "null") + "\n");
    text.append("FM : " + ((faultMember != null) ? faultMember : "null") + "\n");
    text.append("FC : " + faultCode + "\n");
    text.append("DESCRIPTOR : " + ((descriptor != null) ? descriptor : "null") + "\n");
    text.append("USER TIMESTAMP : " + ((userTimestamp != null) ? userTimestamp.toString() : "null") + "\n");
    text.append("USER PROPERTIES : \n");

    if (getUserProperties() == null) {
      text.append("null\n");
    } else {
      Enumeration e = getUserProperties().keys();

      while (e.hasMoreElements()) {
        String name = (String) e.nextElement();
        String value = getUserProperties().getProperty(name);
        text.append(name + " = " + value + "\n");
      }
    }

    return text.toString();
  }

  /** Validate the fault state instance.
   * @throws ASIException if the fault state atributes are null or not valid.
   */
  public void validate() throws ASIException {
    if ((faultFamily == null) || (faultMember == null) || (userTimestamp == null) || (userProperties == null) || (descriptor == null)) {
      throw (new ASIException("null values not allowed"));
    }

    if (!(descriptor.equals(ACTIVE) || descriptor.equals(TERMINATE) || descriptor.equals(CHANGE) || descriptor.equals(INSTANT))) {
      throw (new ASIException("descriptor unknown :" + descriptor));
    }
  }

  /** Return a String representation of the object identity.
   * @return the object identity String representation.
   */
  protected String value() {
    return (faultFamily + "_" + faultMember + "_" + String.valueOf(faultCode));
  }

}

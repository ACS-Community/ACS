package cern.laser.business.data;

import java.io.Serializable;

public class Triplet implements Serializable, Cloneable {
  private String faultFamily;
  private String faultMember;
  private Integer faultCode;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  Triplet() {
  }

  public Triplet(String faultFamily, String faultMember, Integer faultCode) {
    this.faultFamily = faultFamily;
    this.faultMember = faultMember;
    this.faultCode = faultCode;
  }

  //
  // -- PUBLIC STATIC METHODS ---------------------------------------
  //

  public static String toIdentifier(String faultFamily, String faultMember, Integer faultCode) {
    return stringValue(faultFamily, faultMember, faultCode);
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  public String getFaultFamily() {
    return faultFamily;
  }

  public String getFaultMember() {
    return faultMember;
  }

  public Integer getFaultCode() {
    return faultCode;
  }

   void setFaultFamily(String newFaultFamily) {
    faultFamily = newFaultFamily;
  }

   void setFaultMember(String newFaultMember) {
    faultMember = newFaultMember;
  }

   void setFaultCode(Integer newFaultCode) {
    faultCode = newFaultCode;
  }

  public String toIdentifier() {
    return toString();
  }

  public int hashCode() {
    return toString().hashCode();
  }

  public boolean equals(Object obj) {
    if (obj == null) { return false; }
    if (!(obj instanceof Triplet)) { return false; }
    Triplet triplet = (Triplet) obj;

    return toIdentifier().equals(triplet.toIdentifier());
  }

  public Object clone() {
    try {
      Triplet triplet = (Triplet) super.clone();

      return triplet;
    } catch (Exception e) {
      throw new InternalError();
    }
  }

  public String toString() {
    return stringValue(faultFamily, faultMember, faultCode);
  }

  //
  // -- implements XXX ----------------------------------------------
  //

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

  private static final String stringValue(String faultFamily, String faultMember, Integer faultCode) {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append(faultFamily);
    str_buf.append(":");
    str_buf.append(faultMember);
    str_buf.append(":");
    str_buf.append(faultCode);

    return str_buf.toString();
  }
}
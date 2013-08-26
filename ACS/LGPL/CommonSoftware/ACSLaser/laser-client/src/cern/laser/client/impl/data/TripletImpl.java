/*
 * $Id: TripletImpl.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.impl.data;

import java.io.Serializable;

import cern.laser.client.data.Triplet;

public class TripletImpl implements Triplet, Cloneable, Serializable {
  private String faultFamily;
  private String faultMember;
  private Integer faultCode;

  public TripletImpl(cern.laser.business.data.Triplet triplet) {
    if (triplet == null) { throw new IllegalArgumentException("parameter can not be null"); }
    faultFamily = triplet.getFaultFamily();
    faultMember = triplet.getFaultMember();
    faultCode = triplet.getFaultCode();
  }

  public String getFaultFamily() {
    return faultFamily;
  }

  public String getFaultMember() {
    return faultMember;
  }

  public Integer getFaultCode() {
    return faultCode;
  }

  public boolean equals(Object obj) {
    if ((obj == null) || (!(obj instanceof Triplet))) { return false; }

    return toString().equals(obj.toString());
  }

  public int hashCode() {
    return this.toString().hashCode();
  }

  public Object clone() throws CloneNotSupportedException {
    try {
      TripletImpl triplet = (TripletImpl) super.clone();

      return triplet;
    } catch (Exception e) {
      throw new CloneNotSupportedException("clone failed : " + e.getMessage());
    }
  }

  public String toString() {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("[");
    str_buf.append(getFaultFamily());
    str_buf.append("][");
    str_buf.append(getFaultMember());
    str_buf.append("][");
    str_buf.append(getFaultCode());
    str_buf.append("]");

    return str_buf.toString();
  }
}
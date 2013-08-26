/*
 * $Id: CategoryActiveList.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.data;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Iterator;

/**
 * 
 * 
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 */
public class CategoryActiveList implements Serializable {
  private Integer categoryId;
  private HashSet activeAlarmIds;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  public CategoryActiveList(Integer id) {
    categoryId = id;
    activeAlarmIds = new HashSet();
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  /**
   * @return
   */
  public Integer getCategoryId() {
    return categoryId;
  }

  /**
   * @return
   */
  public String[] getActiveAlarms() {
    return activeAlarmIds == null ? null : (String[]) activeAlarmIds.toArray(new String[activeAlarmIds.size()]);
  }

  /**
   * @param alarmId
   */
  public boolean addAlarm(String alarmId) {
    return activeAlarmIds.add(alarmId);
  }

  /**
   * @param identifier
   */
  public boolean removeAlarm(String identifier) {
    return activeAlarmIds.remove(identifier);
  }

  //
  // -- extends Object ----------------------------------------------
  //

  /*
   * (non-Javadoc)
   * 
   * @see java.lang.Object#equals(java.lang.Object)
   */

  public boolean equals(Object obj) {
    if (obj == null) { return false; }
    if (!(obj instanceof CategoryActiveList)) { return false; }
    CategoryActiveList activeList = (CategoryActiveList) obj;
    return getCategoryId().equals(activeList.getCategoryId());
  }

  /*
   * (non-Javadoc)
   * 
   * @see java.lang.Object#hashCode()
   */
  public int hashCode() {
    return getCategoryId().intValue();
  }

  /*
   * (non-Javadoc)
   * 
   * @see java.lang.Object#toString()
   */
  public String toString() {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nACTIVE LIST FOR CATEGORY " + categoryId + " :");
    for (Iterator iter = activeAlarmIds.iterator(); iter.hasNext();) {
      String element = (String) iter.next();
      str_buf.append("\n");
      str_buf.append(element);
    }

    return str_buf.toString();
  }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

}
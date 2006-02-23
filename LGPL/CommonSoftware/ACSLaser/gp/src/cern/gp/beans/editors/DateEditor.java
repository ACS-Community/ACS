/*
 * $Id: DateEditor.java,v 1.1 2005/06/07 03:26:13 kzagar Exp $
 *
 * $Date: 2005/06/07 03:26:13 $
 * $Revision: 1.1 $
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.beans.editors;

/**
 * Read only property editor allowing to represent java.util.Date as a string
 *
 * @version $Revision: 1.1 $  $Date: 2005/06/07 03:26:13 $
 * @author Lionel Mestre
 */
public class DateEditor extends java.beans.PropertyEditorSupport {
  
  private static java.text.DateFormat dateFormat = createDateFormat();
  
  private static java.text.DateFormat createDateFormat() {
    return java.text.DateFormat.getDateTimeInstance(java.text.DateFormat.MEDIUM, java.text.DateFormat.MEDIUM);
  }
  
  
  /** Creates new DateEditor */
  public DateEditor() {
  }
  
  /**
   * get the value as text, after it has been deposited by setValue()
   * @return a string representation of the value
   */
  public String getAsText() {
    java.util.Date date = (java.util.Date) getValue();
    if (date == null) return "unkown";
    return dateFormat.format(date);
  }
  
}

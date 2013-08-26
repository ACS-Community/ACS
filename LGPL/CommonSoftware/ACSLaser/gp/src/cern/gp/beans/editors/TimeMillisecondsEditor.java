/*
 * $Id: TimeMillisecondsEditor.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.beans.editors;

/**
 * Read only property editor allowing to represent a number of milliseconds
 * in a formated string like this one
 *   <seconds> s <milliseconds> ms
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public class TimeMillisecondsEditor extends java.beans.PropertyEditorSupport {
  
  
  /** Creates new TimeMillisecondsEditor */
  public TimeMillisecondsEditor() {
  }
  
  /**
   * get the value as text, after it has been deposited by setValue()
   * @return a string representation of the value
   */
  public String getAsText() {
    Integer timemillis = (Integer) getValue();
    if (timemillis == null) return "unkown";
    int millis = timemillis.intValue();
    int seconds = millis / 1000;
    millis %= 1000;
    if (seconds > 0) {
      return Integer.toString(seconds)+" s " + (millis > 0 ? Integer.toString(millis)+" ms" : "");
    }
    return Integer.toString(millis)+" ms";
  }
  
}

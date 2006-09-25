/*
 * $Id: YesNoEditor.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.beans.editors;

/**
 * Editor that interprets a boolean as yes or no choice.
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Vito Baggiolini
 */
 public class YesNoEditor extends java.beans.PropertyEditorSupport  {
  
  /** the tags recognized by this editor */
  private static final String YES = "yes";
  private static final String NO = "no";
  private static String[] tagStrings = { YES, NO};
  
  public YesNoEditor() {
  }
  
  /**
   * @return the tags recognized by this editor
   */
  public String[] getTags() {
    return tagStrings;
  }
  
  /**
   * used by the IDE for code generation
   */
  public String getJavaInitializationString() {
    return "\"" + getAsText() + "\"";
  }
  
  /**
   * Sets the boolean property from a String.
   * This method interprets the string and converts it into a boolean
   * @param text must be one of the tags
   */
  public void setAsText(String text) throws IllegalArgumentException {
    if (YES.equalsIgnoreCase(text)) {
      this.setValue(new Boolean(true));
    } else if (NO.equalsIgnoreCase(text)){
      this.setValue(new Boolean(false));
    } else {
      throw new IllegalArgumentException("not a recognized Yes/No string: " + text);
    }
  }
  /**
   * return the boolean property as Text.
   * This method returns a string that corresponds to the boolean value
   * @return one of the two tags
   */
  public String getAsText() {
    Object value = getValue();
    if (value == null) { return "<unknown>"; }
    return ((Boolean)getValue()).booleanValue() ? YES : NO;
  }
  
}
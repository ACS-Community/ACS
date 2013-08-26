/*
 * Colored.java
 *
 * Created on September 30, 2002, 2:41 PM
 */

package cern.gp.beans.editors.support;

import java.awt.Color;

/**
 * an interface used together with ColoredEditorHelper, to tell it what color it shall use to paint table cells.
 * typically the main PropertyEditor will implement this interface, and return appropriate colors according
 * to the value of the property it edits.
 *
 * @author  Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public interface ColorMaster {
	/** the background color property name */
	public static final String BACKGROUND_COLOR_PROPERTY_NAME = "backgroundColor";

	/** the foreground color property name */
	public static final String FOREGROUND_COLOR_PROPERTY_NAME = "foregroundColor";
	
	/**
   * the background color to be used or <code>null</code> for no change in color
   */
  public Color getBackgroundColor();
  /** 
   * the foreground color to be used or <code>null</code> for no change in color
   */
  public Color getForegroundColor();
}

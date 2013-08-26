/*
 * $Id: ColoredEditorHelper.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */

package cern.gp.beans.editors.support;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.beans.PropertyEditor;

/**
 * A helper class used to build editors that set the background color of the field they edit. 
 * Normally, this class is used inside an Editor class that cannot inherit from ColoredEditorSupport. 
 * In this case, that editor delegates the paintValue and the isPaintable calls to this class.
 *
 * This uses the Tie-delegation idiom.
 * @author  Vito Baggiolini
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 */
public class ColoredEditorHelper  {
  private Color currentColor;
  private final ColorMaster colorMaster;
  private final PropertyEditor ped;

  /**
   * Constructor 
   * @param ped the property editor, that owns this object, needed to know the text to be displayed
   * @param colorMaster the class that knows what color shall be used
   */ 
  public ColoredEditorHelper(PropertyEditor ped, ColorMaster colorMaster) {
    this.colorMaster = colorMaster;
    this.ped = ped;
  }
  
  /**
   * method that draws the text retrieved from PropertyEditor with the given background given by
   * ColorMaster.
   * This method is delegated to from the Editor
   */
  public void paintValue(Graphics g, Rectangle rectangle) {
    ColoredEditorSupport.paintValue(g, rectangle, ped.getAsText(),
      colorMaster.getBackgroundColor(), colorMaster.getForegroundColor());
  }
  
  
  /**
   * the method that determines whether the text shall be drawn using the paintValue() method
   * This method is delegated to from the Editor
   */
  public boolean isPaintable() {
    return (colorMaster.getBackgroundColor() != null || colorMaster.getForegroundColor() != null);
  }
}

/*
 * ColoredInteger.java
 *
 * Created on September 30, 2002, 2:47 PM
 */

package cern.gp.explorer.test.helpers;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.beans.PropertyEditorSupport;

import cern.gp.beans.editors.support.ColorMaster;
import cern.gp.beans.editors.support.ColoredEditorHelper;


/**
 * @author Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class ColoredIntegerEditor extends PropertyEditorSupport implements ColorMaster {
  private final ColoredEditorHelper helper;
  /** Creates a new instance of ColoredInteger */
  public ColoredIntegerEditor() {
    helper = new ColoredEditorHelper(this, this);
  }
  
  /**
   * sets the Integer value from a text
   */
  public void setAsText(String text) {
    setValue(new Integer(text));
  }
  //---------------- implements ColorMaster ------------------------------------
  /**
   * based on the Integer value, this method returns the background color.
   * THe color is null if the value is 0, cyan if the value is dividable by 2 and magenta if not
   * @return the background color or null for no backgound color
   */
  public Color getBackgroundColor() {
    int myVal = ((Integer)getValue()).intValue();
    if (myVal == 0) {
      return null;
    } else if ((myVal % 2) != 0) {
      return Color.magenta;
    } else {
      return Color.cyan;
    }
  }
  public Color getForegroundColor() {
    return null;
  }
  
  //---------------- delegated to ColorEditorMixin -----------------------------
  public boolean isPaintable(){
    return (helper.isPaintable());
  }
  
  public void paintValue(Graphics gfx, Rectangle rect) {
    helper.paintValue(gfx, rect);
  }
}

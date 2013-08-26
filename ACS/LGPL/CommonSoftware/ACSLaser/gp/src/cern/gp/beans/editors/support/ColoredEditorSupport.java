/*
 * DummyBeanEditor.java
 *
 * Created on September 24, 2002, 8:55 PM
 */

package cern.gp.beans.editors.support;

import java.awt.Color;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.beans.PropertyEditorSupport;

/**
 * An editor base class for building editors that color the field they edit when it has been changed
 * @author Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public abstract class ColoredEditorSupport extends PropertyEditorSupport implements ColorMaster {

  //-------------- overrides PropertyEditor ------------------------------------
  
  public void paintValue(Graphics g, Rectangle rectangle) {
    paintValue(g, rectangle, getAsText(), getBackgroundColor(), getForegroundColor());
  }
  
  public boolean isPaintable() {
    return (getBackgroundColor() != null || getForegroundColor() != null);
  }

  /**
   * a static version of the paintValue method that can be called from other places
   * @param graphics the graphics on which to drow
   * @param rectangle the rectangle to fill
   * @param text the text to display
   */
  public static void paintValue(Graphics g, Rectangle rectangle, String text, Color backGround, Color foreGround) {  
    Color origColor = g.getColor();

    // change background color only if the parameter backGround is non-null
    if (backGround != null) {      
      g.setColor(backGround);
    } 
    g.fillRect(rectangle.x, rectangle.y, rectangle.width, rectangle.height);
    
    // draw the text
    int xOffset = 6;
    if (foreGround != null) {
      g.setColor(foreGround);
    } else {
      g.setColor(Color.black);
    }
    FontMetrics fm = g.getFontMetrics();
    g.drawString(text, rectangle.x + xOffset, rectangle.y + (rectangle.height - fm.getHeight()) / 2 + fm.getAscent());

    // restore color
    g.setColor(origColor); 
  }
  
  
  public abstract Color getBackgroundColor();    
  public abstract Color getForegroundColor();
}

/*
 * $Id: PrintableComponent.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.printing;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterException;

/**
 * 
 * 
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 */
public class PrintableComponent extends Component implements Printable {
  Component component;

  public PrintableComponent(Component component) {
    super();
    this.component = component;
  }

  //
  // -- implements Printable ----------------------------------------------
  //

  /* (non-Javadoc)
   * @see java.awt.print.Printable#print(java.awt.Graphics, java.awt.print.PageFormat, int)
   */
  public int print(Graphics g, PageFormat pageFormat, int pageIndex) throws PrinterException {
    if (pageIndex == 0) {
      Graphics2D g2d = (Graphics2D) g;
      g2d.translate(pageFormat.getImageableX(), pageFormat.getImageableY());

      double pageHeight = pageFormat.getImageableHeight();
      double pageWidth = pageFormat.getImageableWidth();
      double width = (double) component.getWidth();
      double height = (double) component.getHeight();

      double xScale = 1;
      if (width >= pageWidth) {
        xScale = pageWidth / width;
      }
      double yScale = 1;
      if (height >= pageHeight) {
        yScale = pageHeight / height;
      }

      g2d.scale(xScale, yScale);
      component.paint(g2d);
      return Printable.PAGE_EXISTS;
    } else {
      return Printable.NO_SUCH_PAGE;
    }
  }
}

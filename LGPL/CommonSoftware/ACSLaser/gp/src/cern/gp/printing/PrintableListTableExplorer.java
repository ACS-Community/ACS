/*
 * $Id: PrintableListTableExplorer.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.printing;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterException;

import javax.swing.JTable;

import cern.gp.explorer.ListTableExplorer;

/**
 * 
 * 
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 */
public class PrintableListTableExplorer implements Printable {
  protected JTable myTable;

  public PrintableListTableExplorer(ListTableExplorer explorer) {
    myTable = explorer.getTreeTableAccess().getTable();
  }

  //
  // -- implements Printable ----------------------------------------------
  //

  /* (non-Javadoc)
   * @see java.awt.print.Printable#print(java.awt.Graphics, java.awt.print.PageFormat, int)
   */
  public int print(Graphics g, PageFormat pageFormat, int pageIndex) throws PrinterException {
    Graphics2D g2 = (Graphics2D) g;
    //g2.setColor(Color.black);
    int fontHeight = g2.getFontMetrics().getHeight();
    int fontDesent = g2.getFontMetrics().getDescent();

    //leave room for page number
    double pageHeight = pageFormat.getImageableHeight() - fontHeight;
    double pageWidth = pageFormat.getImageableWidth();
    double tableWidth = (double) myTable.getColumnModel().getTotalColumnWidth();
    double scale = 1;
    if (tableWidth >= pageWidth) {
      scale = pageWidth / tableWidth;
    }

    double headerHeightOnPage = myTable.getTableHeader().getHeight() * scale;
    double tableWidthOnPage = tableWidth * scale;

    double oneRowHeight = (myTable.getRowHeight() + myTable.getRowMargin()) * scale;
    int numRowsOnAPage = (int) ((pageHeight - headerHeightOnPage) / oneRowHeight);
    double pageHeightForTable = oneRowHeight * numRowsOnAPage;
    int totalNumPages = (int) Math.ceil(((double) myTable.getRowCount()) / numRowsOnAPage);
    if (pageIndex >= totalNumPages) {
      return NO_SUCH_PAGE;
    }

    g2.translate(pageFormat.getImageableX(), pageFormat.getImageableY());
    g2.drawString("Page: " + (pageIndex + 1), (int) pageWidth / 2 - 35, (int) (pageHeight + fontHeight - fontDesent));
    //bottom center

    g2.translate(0f, headerHeightOnPage);
    g2.translate(0f, -pageIndex * pageHeightForTable);

    //If this piece of the table is smaller than the size available,
    //clip to the appropriate bounds.
    if (pageIndex + 1 == totalNumPages) {
      int lastRowPrinted = numRowsOnAPage * pageIndex;
      int numRowsLeft = myTable.getRowCount() - lastRowPrinted;
      g2.setClip(
        0,
      (int) (pageHeightForTable * pageIndex),
        (int) Math.ceil(tableWidthOnPage),
        (int) Math.ceil(oneRowHeight * numRowsLeft));
    }
    //else clip to the entire area available.
    else {
      g2.setClip(
        0,
      (int) (pageHeightForTable * pageIndex),
        (int) Math.ceil(tableWidthOnPage),
        (int) Math.ceil(pageHeightForTable));
    }

    g2.scale(scale, scale);
    myTable.paint(g2);
    g2.scale(1 / scale, 1 / scale);
    g2.translate(0f, pageIndex * pageHeightForTable);
    g2.translate(0f, -headerHeightOnPage);
    g2.setClip(0, 0, (int) Math.ceil(tableWidthOnPage), (int) Math.ceil(headerHeightOnPage));
    g2.scale(scale, scale);
    myTable.getTableHeader().paint(g2); //paint header at top

    return Printable.PAGE_EXISTS;
  }
}

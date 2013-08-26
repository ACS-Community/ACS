/*
 * $Id: PrintUtil.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.printing;

import java.awt.Component;
import java.awt.print.Printable;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;

import javax.print.DocFlavor;
import javax.print.PrintService;
import javax.print.PrintServiceLookup;
import javax.print.attribute.HashPrintRequestAttributeSet;
import javax.print.attribute.PrintRequestAttributeSet;
import javax.print.attribute.standard.MediaSizeName;
import javax.print.attribute.standard.OrientationRequested;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import cern.gp.explorer.ListTableExplorer;
import cern.gp.explorer.TreeExplorer;
import cern.gp.util.Assertion;

/**
 * 
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 */
public class PrintUtil {
  private static final String PRINTER_NAME = "Acrobat Distiller";

  private static PrintUtil singleton = new PrintUtil();

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  /** Cannot be instantiated */
  private PrintUtil() {
  }

  //
  // -- PUBLIC CLASS METHODS ----------------------------------------
  //

  /**
   * Returns the singleton instance of <code>PrintUtil</code>.
   * @return the singleton instance of <code>PrintUtil</code>
   */
  public static PrintUtil getInstance() {
    return singleton;
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  public void print(Printable printable) {
    printPDF("", printable);
  }

  /* (non-Javadoc)
   * @see cern.gp.printing.PrintStrategy#print(java.awt.print.Printable)
   */
  public void print(Component component) {
    Printable printable = null;

    if (component instanceof TreeExplorer) {
      printable = new PrintableTreeExplorer((TreeExplorer) component);
    } else if (component instanceof ListTableExplorer) {
      printable = new PrintableListTableExplorer((ListTableExplorer) component);
    } else {
      printable = new PrintableComponent(component);
    }

    printPDF(component.getName(), printable);
  }

  //
  //  -- PRIVATE METHODS ---------------------------------------------
  //

  private void printPDF(String name, Printable printable) {
    DocFlavor flavor = DocFlavor.SERVICE_FORMATTED.PRINTABLE;
    PrintRequestAttributeSet aset = new HashPrintRequestAttributeSet();
    aset.add(MediaSizeName.ISO_A4);
    aset.add(OrientationRequested.PORTRAIT);

    PrintService service = findPrinterService(flavor, aset);
    Assertion.assertTrue(service != null, "service != null");

    PrinterJob pj = PrinterJob.getPrinterJob();
    try {
      pj.setPrintService(service);
      pj.setPrintable(printable);

      pj.pageDialog(aset);
      pj.print(aset);
    } catch (PrinterException e) {
      Log log = LogFactory.getLog(PrintUtil.class);
      log.warn("The component " + name + " could not be printed");
    }
  }

  /* (non-javadoc)
   * Locates among the available print services the one with the name defined by PRINTER_NAME. 
   * If that service is not available, the default print service is returned.
   */
  private PrintService findPrinterService(DocFlavor flavor, PrintRequestAttributeSet aset) {
    PrintService[] services = PrintServiceLookup.lookupPrintServices(flavor, aset);
    for (int i = 0; i < services.length; i++) {
      if (services[i].getName().compareToIgnoreCase(PRINTER_NAME) == 0)
        return services[i];
    }

    return PrintServiceLookup.lookupDefaultPrintService();
  }
}

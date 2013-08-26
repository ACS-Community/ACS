/*
 * $Id: GPManager.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.util;

import org.openide.ErrorManager;

/**
 * This class must be used as a replacement for <code>TopManager</code> whenever <code>TopManager</code> has to be used.
 * It provides the same utility methods for getting services from NetBeans. New versions of NetBeans removed
 * <code>TopManager</code> completely and are changing the way those services are accessed. This class limits the impact
 * of the changes in your code.
 * 
 * @author Lionel Mestre
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class GPManager {

  /** Serious problem, application may be crippled */
  public final static int ERROR = ErrorManager.ERROR;

  /** Something went wrong, though it can be recovered */
  public final static int EXCEPTION = ErrorManager.EXCEPTION;

  /** Message that would be useful for tracing events but which need not be a problem */
  public final static int INFORMATIONAL = ErrorManager.INFORMATIONAL;

  /** Undefined severity */
  public final static int UNKNOWN = ErrorManager.UNKNOWN;

  /** Something the user should be aware of */
  public final static int USER = ErrorManager.USER;

  /** Something went wrong in the software, but it is continuing and the user need not be bothered */
  public final static int WARNING = ErrorManager.WARNING;

  /** Show text in the IDE's status line.
   * Can be called at any time, but remember the text may not be updated
   * until the AWT event queue is ready for it - so if you are hogging
   * the event queue the text will not appear until you release it
   * (finish your work or display a modal dialog, for example).
   * @param text the text to be shown
   */
  public static final void setStatusText(String text) {
    org.openide.awt.StatusDisplayer.getDefault().setStatusText(text);
  }

  /** Get a new standard dialog.
  * The dialog is designed and created as specified in the parameter.
  * Anyone who wants a dialog with standard buttons and
  * standard behavior should use this method.
  * <p><strong>Do not cache</strong> the resulting dialog if it
  * is modal and try to reuse it! Always create a new dialog
  * using this method if you need to show a dialog again.
  * Otherwise previously closed windows can reappear.
  *
  * @param descriptor general description of the dialog
  */
  public static final java.awt.Dialog createDialog(org.openide.DialogDescriptor descriptor) {
    return org.openide.DialogDisplayer.getDefault().createDialog(descriptor);
  }

  /** Notify the user of something in a message box, possibly with feedback.
   * <p>To support both GUI and non-GUI use, this method may be called
   * from any thread (providing you are not holding any locks), and
   * will block the caller's thread. In GUI mode, it will be run in the AWT
   * event thread automatically. If you wish to hold locks, or do not
   * need the result object immediately or at all, please make this call
   * asynchronously (e.g. from the request processor).
   * @param nd description of the notification
   * @return the option that caused the message box to be closed
   */
  public static final Object notify(org.openide.NotifyDescriptor descriptor) {
    return org.openide.DialogDisplayer.getDefault().notify(descriptor);
  }

  /** Support writing to the Output Window on the main tab.
   * @return a writer for the standard IDE output
   */
  public static final org.openide.windows.OutputWriter getStdOut() {
    return org.openide.windows.IOProvider.getDefault().getStdOut();
  }

  /** Support reading from and writing to a specific tab on the Output Window.
   * If a tab with the supplied name already exists, a new tab with the same name will be created regardless.
   * @param name desired name of the tab
   * @return an <code>InputOutput</code> class for accessing the new tab
   */
  public static final org.openide.windows.InputOutput getIO(String name, boolean newIO) {
    return org.openide.windows.IOProvider.getDefault().getIO(name, newIO);
  }

  /**
   * Prints the exception to the log file and (possibly) notifies the user.
   * @param everity the severity to be applied to the exception (overrides default), e.g. EXCEPTION
   * @param t the exception to notify
   */
  public static final void notify(int severity, Throwable t) {
    ErrorManager.getDefault().notify(severity, t);
  }

  /**
   * Browse a document named by some URL. HtmlBrowser provides more control in certain cases.
   * @param url URL of WWW or local document to be shown
   * @param t the exception to notify
   */
  public static final void showURL(java.net.URL url) {
    org.openide.awt.HtmlBrowser.URLDisplayer.getDefault().showURL(url);
  }

}

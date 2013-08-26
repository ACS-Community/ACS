/*
 * $Id: ProcessingController.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * This class is a singleton controlling if the MDB's should start processing alarm messages or not.
 * The MDB's are notified via an callback method.
 * 
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 */
public class ProcessingController {

  private static ProcessingController singleton;
  private Set listeners;
  private boolean isProcessing;
  
  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  private ProcessingController() {
    listeners = new HashSet();
    isProcessing = false;
  }
  
  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  public static final synchronized ProcessingController getInstance() {
    if (singleton == null) {
      singleton = new ProcessingController();
    }
    
    return singleton;
  }
  
  public synchronized void addListener(ProcessingControllerListener listener) {
    listeners.add(listener);
  }
  
  public synchronized void startProcessing() {
    isProcessing = true;
    for (Iterator iter = listeners.iterator(); iter.hasNext();) {
      ProcessingControllerListener listener = (ProcessingControllerListener) iter.next();
      listener.startProcessing();
    }
  }
    
  public synchronized boolean isProcessing() {
    return isProcessing;
  }

  //
  // -- implements XXX ----------------------------------------------
  //

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //
}

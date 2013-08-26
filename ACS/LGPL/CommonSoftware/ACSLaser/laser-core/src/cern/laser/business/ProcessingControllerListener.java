/*
 * $Id: ProcessingControllerListener.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business;

/**
 * The interface to implement to be notified when to start processing alarm messages.
 * 
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 */
public interface ProcessingControllerListener {
  public void startProcessing();
}

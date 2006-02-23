/*
 * $Id: ProcessingControllerListener.java,v 1.1.1.1 2005/03/30 13:37:50 acaproni Exp $
 *
 * $Date: 2005/03/30 13:37:50 $ 
 * $Revision: 1.1.1.1 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business;

/**
 * The interface to implement to be notified when to start processing alarm messages.
 * 
 * @version $Revision: 1.1.1.1 $ $Date: 2005/03/30 13:37:50 $
 * @author Katarina Sigerud
 */
public interface ProcessingControllerListener {
  public void startProcessing();
}

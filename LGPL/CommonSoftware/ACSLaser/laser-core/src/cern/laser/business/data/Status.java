/*
 * $Id: Status.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.data;

import java.sql.Timestamp;
import java.util.Properties;

/**
 * 
 * 
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 */
public interface Status {
  public static final Status INITIAL_STATUS = new StatusImpl(Boolean.FALSE, Boolean.FALSE, Boolean.FALSE, Boolean.FALSE,
      Boolean.FALSE,
      /* Boolean.FALSE, */"UNDEFINED", new Timestamp(0), new Timestamp(0), new Timestamp(0), new Properties());
  
  public String getStatusId();

  public void setStatusId(String alarmId);

  /**
   * @return Boolean
   */
  public Boolean getActive();

  /**
   *  
   */
  public Boolean getMasked();

  /**
   *  
   */
  public Boolean getReduced();

  /**
   *  
   */
  public Boolean getActivatedByBackup();

  public void setActivatedByBackup(Boolean newActiveByBackup);

  /**
   */
  public Boolean getTerminatedByBackup();

  public void setTerminatedByBackup(Boolean newTerminatedByBackup);

  //  /**
  public String getSourceHostname();

  /**
   * 
   * @return Timestamp
   * @return
   */
  public Timestamp getSourceTimestamp();

  /**
   * 
   * @return Timestamp
   * @return
   */
  public Timestamp getUserTimestamp();

  public void setUserTimestamp(Timestamp newUserTimestamp);

  /**
   * 
   * @return Timestamp
   * @return
   */
  public Timestamp getSystemTimestamp();

  public void setActive(Boolean newActive);

  public void setMasked(Boolean newMasked);

  public void setReduced(Boolean newReduced);

  public void setSourceHostname(String newSourceHostname);

  public void setSourceTimestamp(Timestamp newSourceTimestamp);

  public void setSystemTimestamp(Timestamp newSystemTimestamp);

  public Properties getProperties();

  public void setProperties(Properties newProperties);
}
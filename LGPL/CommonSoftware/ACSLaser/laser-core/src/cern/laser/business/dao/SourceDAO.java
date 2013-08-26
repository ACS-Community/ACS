/*
 * $Id: SourceDAO.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.3 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.dao;

import cern.laser.business.data.Source;

/**
 * 
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 */
public interface SourceDAO {
  //public void setLaserSourceId(String laserSourceId);
  
  public Source findSource(String sourceId);

  public Source getSource(String sourceId);
  
  public Source[] findAllSources();
  
  public Source findByLaserSource();
  
  public void saveSource(Source source);
  
  public void deleteSource(Source source);
  
  public void updateSource(Source source);
  
  public String[] getAlarms(String sourceId);
}
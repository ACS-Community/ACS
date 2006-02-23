/*
 * $Id: SourceDAO.java,v 1.2 2005/04/18 08:48:50 kzagar Exp $
 *
 * $Date: 2005/04/18 08:48:50 $ 
 * $Revision: 1.2 $ 
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.dao;

import cern.laser.business.data.Source;

/**
 * 
 * 
 * @version $Revision: 1.2 $ $Date: 2005/04/18 08:48:50 $
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
/*
 * $Id: ResponsiblePersonDAO.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.3 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.dao;

import cern.laser.business.data.ResponsiblePerson;

/**
 * 
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 */
public interface ResponsiblePersonDAO {
  //public ResponsiblePerson findResponsiblePerson(Integer identifier);
  
  public ResponsiblePerson getResponsiblePerson(Integer identifier);
  
  public String[] getAlarms(Integer responsibleId);
  
  public ResponsiblePerson[] findAllResponsiblePersons();
}
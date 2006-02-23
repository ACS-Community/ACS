/*
 * $Id: ResponsiblePersonDAO.java,v 1.2 2005/04/18 08:48:50 kzagar Exp $
 *
 * $Date: 2005/04/18 08:48:50 $ 
 * $Revision: 1.2 $ 
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.dao;

import cern.laser.business.data.ResponsiblePerson;

/**
 * 
 * 
 * @version $Revision: 1.2 $ $Date: 2005/04/18 08:48:50 $
 * @author Katarina Sigerud
 */
public interface ResponsiblePersonDAO {
  //public ResponsiblePerson findResponsiblePerson(Integer identifier);
  
  public ResponsiblePerson getResponsiblePerson(Integer identifier);
  
  public String[] getAlarms(Integer responsibleId);
  
  public ResponsiblePerson[] findAllResponsiblePersons();
}
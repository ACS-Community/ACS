/*
 * $Id: CategoryBrowsingHandlerImpl.java,v 1.10 2011/04/13 15:45:42 acaproni Exp $
 *
 * $Date: 2011/04/13 15:45:42 $ 
 * $Revision: 1.10 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.impl.services.browsing;

import java.util.ArrayList;
import java.util.Collection;

import org.omg.CORBA.ORB;

import cern.laser.client.LaserConnectionException;
import cern.laser.client.LaserException;
import cern.laser.client.data.Category;
import cern.laser.client.impl.common.AlarmServiceSingleton;
import cern.laser.client.impl.data.CategoryImpl;
import cern.laser.client.services.browsing.CategoryBrowsingHandler;

import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogger;
import alma.alarmsystem.CERNAlarmService;

public class CategoryBrowsingHandlerImpl extends CategoryBrowsingHandler {
    
  // The AlarmService component
  private CERNAlarmService m_laser;
  
  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  public CategoryBrowsingHandlerImpl(ORB orb, AcsLogger logger) throws LaserConnectionException {
	  try {
		  this.m_laser = AlarmServiceSingleton.getInstance(orb,logger);
	  } catch (Throwable t) {
		  throw new LaserConnectionException("Error getting the alarm service",t);
	  }
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  //
  // -- extends CategoryBrowsingHandler------------------------------
  //

//  public Category getCategoryById(Integer id) throws LaserException {
//    return null;
//  }

  public Category getCategoryByPath(String path) throws LaserException {
    javax.swing.JOptionPane.showMessageDialog(null,"("+path+")","getCategoryByPath",javax.swing.JOptionPane.INFORMATION_MESSAGE);
  	
    if (path == null) { throw new IllegalArgumentException("category path cannot be null"); }
    if (path.startsWith(".")) { throw new IllegalArgumentException("category path cannot start with '.'"); }
    if (path.endsWith(".")) { throw new IllegalArgumentException("category path cannot end with '.'"); }

  	if (m_laser!=null) {
        alma.alarmsystem.Category c = m_laser.getCategoryByPath(path);
        cern.laser.business.data.Category categoryBusiness = toBusinessCategory(c);
        return new CategoryImpl(categoryBusiness);
    } else {
        throw new NullPointerException("AlarmSystem component is null");
    }
  }

  public Category getCategoryTreeRoot() throws LaserException {
  	if (m_laser!=null) {
        alma.alarmsystem.Category c = m_laser.getCategoryTreeRoot();
        cern.laser.business.data.Category categoryBusiness = toBusinessCategory(c);
        return new CategoryImpl(categoryBusiness);
    } else {
        throw new NullPointerException("AlarmSystem component is null");
    }
  }

  public Collection getChildren(Category node) throws LaserException {
    if (node == null) { throw new IllegalArgumentException("parameter cannot be null"); }
    return getChildren(node.getCategoryId());
  }

  public Collection getChildren(Integer nodeId) throws LaserException {
  	if (m_laser!=null) {
        alma.alarmsystem.Category[] cs = m_laser.getCategoryChildren(nodeId.intValue());
        return toBusinessCategoryCollection(cs);
    } else {
        throw new NullPointerException("AlarmSystem component is null");
    }
  }

 public Category getParent(Category node) throws LaserException {
    if (node == null) { throw new IllegalArgumentException("parameter cannot be null"); }

    return getParent(node.getCategoryId());
  }

  public Category getParent(Integer nodeId) throws LaserException {
  	if (m_laser!=null) {
        alma.alarmsystem.Category c = m_laser.getCategoryParent(nodeId.intValue());
        cern.laser.business.data.Category categoryBusiness = toBusinessCategory(c);
        return new CategoryImpl(categoryBusiness);
    } else {
        throw new NullPointerException("AlarmSystem component is null");
    }
  }

  public Collection getCategories() throws LaserException {
  	if (m_laser!=null) {
        alma.alarmsystem.Category[] cs = m_laser.getCategories();
        return toBusinessCategoryCollection(cs);
    } else {
        throw new NullPointerException("AlarmSystem component is null");
    }
  }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //
  
  /**
   * @param c
   * @return
   */
  private static cern.laser.business.data.Category toBusinessCategory(alma.alarmsystem.Category c)
  {
  		return new cern.laser.business.data.CategoryImpl(
  				new Integer(c.categoryId), c.name,
				c.description, c.path, c.leaf);
  }

  /**
   * Helper method.
   * @param cs
   * @return
   */
  public static Collection toBusinessCategoryCollection(alma.alarmsystem.Category[] cs) {
      if (cs == null || cs.length == 0)
      	return new ArrayList(0);

    Collection result = new ArrayList(cs.length);
  	for (int i = 0; i < cs.length; i++) {
  	    cern.laser.business.data.Category categoryBusiness = toBusinessCategory(cs[i]);
  		result.add(new CategoryImpl(categoryBusiness));
  	}
  	return result;
  }

}
/*
 * $Id: CategoryBrowsingHandler.java,v 1.3 2011/02/13 15:37:17 acaproni Exp $
 *
 * $Date: 2011/02/13 15:37:17 $ 
 * $Revision: 1.3 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.services.browsing;

import java.util.Collection;

import alma.acs.container.ContainerServicesBase;

import cern.laser.client.LaserConnectionException;
import cern.laser.client.LaserException;
import cern.laser.client.data.Category;
import cern.laser.client.impl.services.browsing.CategoryBrowsingHandlerImpl;

/**
 * Provides the service to handle alarm category hierarchy browsing.
 * 
 * @author F.Calderini
 * @see cern.laser.client.services.browsing.AlarmBrowsingHandler
 * @see cern.laser.client.services.reduction.AlarmReductionHandler
 * @see cern.laser.client.services.selection.AlarmSelectionHandler
 */
public abstract class CategoryBrowsingHandler {
  private static final ThreadLocal categoryBrowsingHandler = new ThreadLocal();

  /**
   * Factory method.
   * 
   * @return an instance of the implementation class
   * @throws LaserException if the request can not be served
   */
  public static CategoryBrowsingHandler get(ContainerServicesBase contSvcs) throws LaserConnectionException {
    CategoryBrowsingHandler instance = (CategoryBrowsingHandler) categoryBrowsingHandler.get();
    if (instance == null) {
      instance = new CategoryBrowsingHandlerImpl(contSvcs);
      categoryBrowsingHandler.set(instance);
    }

    return instance;
  }

  /**
   * Get the category.
   * 
   * @param path the category path in the form <I>name.name....name </I>
   * @throws LaserException if the request can not be served
   * @return the category object, null if the category does not exist
   */
  public abstract Category getCategoryByPath(String path) throws LaserException;

  /**
   * Get the category tree root.
   * 
   * @throws LaserException if the request can not be served
   * @return the category tree root object
   */
  public abstract Category getCategoryTreeRoot() throws LaserException;

  /**
   * Get the category children.
   * 
   * @param node the category parent
   * @throws LaserException if the request can not be served
   * @return the category children collection
   */
  public abstract Collection getChildren(Category node) throws LaserException;

  /**
   * Get the category children.
   * 
   * @param nodeId the category parent identifier
   * @throws LaserException if the request can not be served
   * @return the category children collection
   */
  public abstract Collection getChildren(Integer nodeId) throws LaserException;

  /**
   * Get the category parent.
   * 
   * @param node the category
   * @throws LaserException if the request can not be served
   * @return the parent category, null if the category is the root category
   */
  public abstract Category getParent(Category node) throws LaserException;

  /**
   * Get the category parent.
   * 
   * @param nodeId the category identifier
   * @throws LaserException if the request can not be served
   * @return the parent category, null if the category is the root category
   */
  public abstract Category getParent(Integer nodeId) throws LaserException;

  /**
   * Get all the defined categories.
   * 
   * @throws LaserException if the request can not be served
   * @return the defined category collection
   */
  public abstract Collection getCategories() throws LaserException;
}
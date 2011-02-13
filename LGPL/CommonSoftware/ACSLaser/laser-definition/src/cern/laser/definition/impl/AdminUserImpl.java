package cern.laser.definition.impl;

import java.util.Collection;

import alma.acs.container.ContainerServicesBase;

import cern.laser.definition.AdminUser;
import cern.laser.definition.AlarmDefinitionHandler;
import cern.laser.definition.CategoryDefinitionHandler;
import cern.laser.definition.CategoryLinkDefinitionHandler;
import cern.laser.definition.LaserDefinitionException;
import cern.laser.definition.ReductionMaskDefinitionHandler;
import cern.laser.definition.SourceDefinitionHandler;

/**
 * DOCUMENT ME!
 * 
 * @author $author$
 * @version $Revision: 1.3 $
 */
public class AdminUserImpl implements AdminUser {
  private AlarmDefinitionHandler alarmHandler;
  private CategoryDefinitionHandler categoryHandler;
  private CategoryLinkDefinitionHandler categoryLinkHandler;
  private String userId;
  private ReductionMaskDefinitionHandler reductionHandler;
  private SourceDefinitionHandler sourceHandler;

  /**
   * Creates a new AdminUserImpl object.
   * 
   * @param id DOCUMENT ME!
   */
  public AdminUserImpl(String id) {
    if (id == null) { throw new IllegalArgumentException("argument can not be null"); }

    userId = id;
    alarmHandler = null;
    sourceHandler = null;
    categoryHandler = null;
    reductionHandler = null;
    categoryLinkHandler = null;
  }

  /**
   * DOCUMENT ME!
   * 
   * @return DOCUMENT ME!
   */
  public AlarmDefinitionHandler getAlarmDefinitionHandler(ContainerServicesBase contSvcs) throws LaserDefinitionException {
    if (alarmHandler == null) {
      alarmHandler = new AlarmDefinitionHandlerImpl(userId,contSvcs);
    }

    return alarmHandler;
  }

  /**
   * DOCUMENT ME!
   * 
   * @return DOCUMENT ME!
   */
  public CategoryDefinitionHandler getCategoryDefinitionHandler(ContainerServicesBase contSvcs) throws LaserDefinitionException {
    if (categoryHandler == null) {
      categoryHandler = new CategoryDefinitionHandlerImpl(userId,contSvcs);
    }

    return categoryHandler;
  }

  /**
   * DOCUMENT ME!
   * 
   * @return DOCUMENT ME!
   */
  public CategoryLinkDefinitionHandler getCategoryLinkDefinitionHandler() throws LaserDefinitionException {
    if (categoryLinkHandler == null) {
      categoryLinkHandler = new CategoryLinkDefinitionHandlerImpl(userId);
    }

    return categoryLinkHandler;
  }

  /**
   * DOCUMENT ME!
   * 
   * @return DOCUMENT ME!
   */
  public ReductionMaskDefinitionHandler getReductionMaskDefinitionHandler() throws LaserDefinitionException {
    if (reductionHandler == null) {
      reductionHandler = new ReductionMaskDefinitionHandlerImpl(userId);
    }

    return reductionHandler;
  }

  /**
   * DOCUMENT ME!
   * 
   * @return DOCUMENT ME!
   */
  public SourceDefinitionHandler getSourceDefinitionHandler(ContainerServicesBase contSvcs) throws LaserDefinitionException {
    if (sourceHandler == null) {
      sourceHandler = new SourceDefinitionHandlerImpl(userId,contSvcs);
    }

    return sourceHandler;
  }

  /**
   * DOCUMENT ME!
   * 
   * @return DOCUMENT ME!
   * 
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws UnsupportedOperationException DOCUMENT ME!
   */
  public Collection getUserAlarms() throws LaserDefinitionException {
    throw new UnsupportedOperationException("not implemented");
  }

  /**
   * DOCUMENT ME!
   * 
   * @return DOCUMENT ME!
   * 
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws UnsupportedOperationException DOCUMENT ME!
   */
  public Collection getUserCategories() throws LaserDefinitionException {
    throw new UnsupportedOperationException("not implemented");
  }

  /**
   * DOCUMENT ME!
   * 
   * @return DOCUMENT ME!
   * 
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws UnsupportedOperationException DOCUMENT ME!
   */
  public Collection getUserSources() throws LaserDefinitionException {
    throw new UnsupportedOperationException("not implemented");
  }
}
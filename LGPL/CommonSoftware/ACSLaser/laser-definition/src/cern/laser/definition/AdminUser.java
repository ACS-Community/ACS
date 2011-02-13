package cern.laser.definition;

import java.util.Collection;

import alma.acs.container.ContainerServicesBase;


/** An adminitrative user. It can create, update and remove
 * alarm definitions from the system.
 * @see cern.laser.definition.AdminUserHandler
 */
public interface AdminUser {
  /** Get the alarm definition handler.
 * @return the alarm definition handler.
 */
  public AlarmDefinitionHandler getAlarmDefinitionHandler(ContainerServicesBase contSvcs) throws LaserDefinitionException;

  /** Get the category definition handler.
 * @return the category definition handler.
 */
  public CategoryDefinitionHandler getCategoryDefinitionHandler(ContainerServicesBase contSvcs) throws LaserDefinitionException;

  /** Get the alarm/category link definition handler.
 * @return the alarm/category link definition handler.
 */
  public CategoryLinkDefinitionHandler getCategoryLinkDefinitionHandler() throws LaserDefinitionException;

  /** Get the reduction and mask definition handler.
 * @return the reduction and mask  definition handler.
 */
  public ReductionMaskDefinitionHandler getReductionMaskDefinitionHandler() throws LaserDefinitionException;

  /** Get the source definition handler.
 * @return the source definition handler.
 */
  public SourceDefinitionHandler getSourceDefinitionHandler(ContainerServicesBase contSvcs) throws LaserDefinitionException;

  /** Get the user defined alarms.
 * @return the user defined alarms.
 * @throws LaserDefinitionException if the request can not be served
 */
  public Collection getUserAlarms() throws LaserDefinitionException;

  /** Get the user defined categories.
 * @return the user defined categories.
 * @throws LaserDefinitionException if the request can not be served
 */
  public Collection getUserCategories() throws LaserDefinitionException;

  /** Get the user defined sources.
 * @return the user defined sources.
 * @throws LaserDefinitionException if the request can not be served
 */
  public Collection getUserSources() throws LaserDefinitionException;
}

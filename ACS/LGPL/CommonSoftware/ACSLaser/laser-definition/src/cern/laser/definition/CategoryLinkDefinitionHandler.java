package cern.laser.definition;

import java.io.Reader;
import java.io.Writer;
import java.util.Collection;

import cern.laser.business.definition.data.CategoryLink;


/** Provides the service to handle the alarms by category grouping. Alarm category
 * grouping can be performed from any admin user on any alarm and on any category.
 * The only constraint is that a category has to be linked to all of the alarms involved
 * in a reduction grouping.
 * @see cern.laser.definition.AdminUser
 */
public interface CategoryLinkDefinitionHandler {
  /** Create a new alarm-category link.
   * @param link the alarm-category link
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void createCategoryLink(CategoryLink link) throws LaserDefinitionException;

  /** Dump the user definitions in XML format.
   * @param xmlDefinitionsWriter the XML definitions writer
   * @throws LaserDefinitionXMLException if the XML marshalling failed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void download(Writer xmlDefinitionsWriter) throws LaserDefinitionException;

  /** Remove an alarm-category link.
   * @param link the alarm-category link
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void removeCategoryLink(CategoryLink link) throws LaserDefinitionException;

  /** Execute a bulk update within one single transaction.
   * @param toBeCreated the definitions to create
   * @param toBeRemoved the definitions to remove
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotFoundException if the definition was not found
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void upload(Collection toBeCreated, Collection toBeRemoved) throws LaserDefinitionException;

  /** Execute a bulk update within one single transaction.
   * @param xmlDefinitionsReader the XML definitions reader
   * @throws LaserDefinitionXMLException if the XML unmarshalling failed
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotFoundException if the definition was not found
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void upload(Reader xmlDefinitionsReader) throws LaserDefinitionException;
}

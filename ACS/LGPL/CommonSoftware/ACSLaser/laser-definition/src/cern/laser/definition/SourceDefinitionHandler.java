package cern.laser.definition;

import java.io.Reader;
import java.io.Writer;
import java.util.Collection;

import cern.laser.business.definition.data.SourceDefinition;


/** Provides the service to handle alarm source definitions.
 * A user can be responsible for one ore more sources and a source can be
 * administered by one ore more user. Only the source administrators can update/remove
 * its definition and create/update/remove alarm definitons for that source.
 * @see cern.laser.definition.AdminUser
 */
public interface SourceDefinitionHandler {
  /** Create a new source definition.
   * @param definition the source definition
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void createSource(SourceDefinition definition) throws LaserDefinitionException;

  /** Dump the user definitions in XML format.
   * @param xmlDefinitionsWriter the XML definitions writer
   * @throws LaserDefinitionXMLException if the XML marshalling failed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void download(Writer xmlDefinitionsWriter) throws LaserDefinitionException;

  /** Remove a source definition by its identifier.
   * @param definition the source identifier
   * @throws LaserDefinitionNotFoundException if the source definition was not found
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void removeSource(SourceDefinition definition) throws LaserDefinitionException;

  /** Update a source definition.
   * @param definition the new source definition
   * @throws LaserDefinitionNotFoundException if the source definition was not found
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void updateSource(SourceDefinition definition) throws LaserDefinitionException;

  /** Execute a bulk update within one single transaction.
   * @param toBeCreated the definitions to create
   * @param toBeUpdated the definitions to update
   * @param toBeRemoved the definitions to remove
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotFoundException if the definition was not found
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void upload(Collection toBeCreated, Collection toBeUpdated, Collection toBeRemoved) throws LaserDefinitionException;

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

package cern.laser.definition;

import java.io.Reader;
import java.io.Writer;
import java.util.Collection;

import cern.laser.business.definition.data.AlarmDefinition;


/** Provides the service to handle alarm definitions. Alarm definitions
 * are partitioned with respect to alarm sources. Alarm definition handling permissions
 * inherit from the respective source permissions.
 * @see cern.laser.definition.AdminUser
 */
public interface AlarmDefinitionHandler {
  /** Create a new alarm definition.
   * @param definition the alarm definition
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void createAlarm(AlarmDefinition definition) throws LaserDefinitionException;

  /** Dump the user definitions in XML format.
   * @param xmlDefinitionsWriter the XML definitions writer
   * @throws LaserDefinitionXMLException if the XML marshalling failed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void download(Writer xmlDefinitionsWriter) throws LaserDefinitionException;

  /** Remove an alarm definition by its identifier.
   * @param definition the alarm definition
   * @throws LaserDefinitionNotFoundException if the alarm definition was not found
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void removeAlarm(AlarmDefinition definition) throws LaserDefinitionException;

  /** Update an alarm definition.
   * @param definition the new alarm definition
   * @throws LaserDefinitionNotFoundException if the alarm definition was not found
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void updateAlarm(AlarmDefinition definition) throws LaserDefinitionException;

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

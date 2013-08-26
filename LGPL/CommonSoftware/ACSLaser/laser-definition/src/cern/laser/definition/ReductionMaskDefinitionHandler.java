package cern.laser.definition;

import java.io.Reader;
import java.io.Writer;
import java.util.Collection;

import cern.laser.business.definition.data.AlarmDefinition;
import cern.laser.business.definition.data.MaintenanceMask;
import cern.laser.business.definition.data.ModeMask;
import cern.laser.business.definition.data.MultiplicityThreshold;
import cern.laser.business.definition.data.ReductionLink;


/** Provides the service to handle alarm reduction/mask definitions.
 * Reduction relationships are not partitioned with respect to the administrative
 * users. For mask definitions, the relative alarm permissions apply. The only
 * constraint that applies to reduction links definition is that all the involved
 * alarms have to be linked to the same set of categories.
 * @see cern.laser.definition.AdminUser
 */
public interface ReductionMaskDefinitionHandler {
  /** Set a multiplicity node threshold.
   * @param threshold the multiplicity threshold
   * @throws LaserDefinitionNotValidException if the threshold failed validation
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionNotFoundException if the alarm definition is not found
   * @throws LaserDefinitionException if the request can not be served
   */
  public void setMultiplicityThreshold(MultiplicityThreshold threshold) throws LaserDefinitionException;

  /** Add a new maintenance mask definition.
   * @param alarm the alarm
   * @param maintenance the maintenance mask
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionNotFoundException if the alarm definition is not found
   * @throws LaserDefinitionException if the request can not be served
   */
  public void addMaintenanceMask(AlarmDefinition alarm, MaintenanceMask maintenance) throws LaserDefinitionException;

  /** Add a new mode mask definition.
   * @param alarm the alarm
   * @param mode the mode mask
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionNotFoundException if the alarm definition is not found
   * @throws LaserDefinitionException if the request can not be served
   */
  public void addModeMask(AlarmDefinition alarm, ModeMask mode) throws LaserDefinitionException;

  /** Create a new multiplicity reduction link definition.
   * @param link the reduction link definition
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void createMultiplicityLink(ReductionLink link) throws LaserDefinitionException;

  /** Create a new node reduction link definition.
   * @param link the reduction link definition
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void createNodeLink(ReductionLink link) throws LaserDefinitionException;

  /** Dump the user definitions in XML format.
   * @param xmlDefinitionsWriter the XML definitions writer
   * @throws LaserDefinitionXMLException if the XML marshalling failed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void download(Writer xmlDefinitionsWriter) throws LaserDefinitionException;

  /** Remove a maintenance mask definition.
   * @param alarm the alarm
   * @param maintenance the maintenance mask
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionNotFoundException if the alarm definition is not found
   * @throws LaserDefinitionException if the request can not be served
   */
  public void removeMaintenanceMask(AlarmDefinition alarm, MaintenanceMask maintenance) throws LaserDefinitionException;

  /** Remove a mode mask definition.
   * @param alarm the alarm
   * @param mode the mode mask
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionNotFoundException if the alarm definition is not found
   * @throws LaserDefinitionException if the request can not be served
   */
  public void removeModeMask(AlarmDefinition alarm, ModeMask mode) throws LaserDefinitionException;

  /** Remove a multiplicity link definition.
   * @param link the reduction link definition
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void removeMultiplicityLink(ReductionLink link) throws LaserDefinitionException;

  /** Remove a node reduction link definition.
   * @param link the reduction link definition
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void removeNodeLink(ReductionLink link) throws LaserDefinitionException;

  /** Execute a bulk update within one single transaction.
   * @param toBeCreated the definitions to create
   * @param toBeRemoved the definitions to remove
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotFoundException if the definition was not found
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void uploadNode(Collection toBeCreated, Collection toBeRemoved) throws LaserDefinitionException;

  /** Execute a bulk update within one single transaction.
   * @param toBeCreated the definitions to create
   * @param toBeRemoved the definitions to remove
   * @param thresholds the multiplicity thresholds
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotFoundException if the definition was not found
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void uploadMultiplicity(Collection toBeCreated, Collection toBeRemoved, Collection thresholds) throws LaserDefinitionException;

  /** Execute a bulk update within one single transaction.
   * @param toBeCreated the definitions to create
   * @param toBeRemoved the definitions to remove
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotFoundException if the definition was not found
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void uploadMaintenance(Collection toBeCreated, Collection toBeRemoved) throws LaserDefinitionException;

  /** Execute a bulk update within one single transaction.
   * @param toBeCreated the definitions to create
   * @param toBeRemoved the definitions to remove
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotFoundException if the definition was not found
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void uploadMode(Collection toBeCreated, Collection toBeRemoved) throws LaserDefinitionException;

  /** Execute a bulk update within one single transaction.
   * @param xmlDefinitionsReader the XML definitions reader
   * @throws LaserDefinitionXMLException if the XML unmarshalling failed
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotFoundException if the definition was not found
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void uploadReduction(Reader xmlDefinitionsReader) throws LaserDefinitionException;

  /** Execute a bulk update within one single transaction.
   * @param xmlDefinitionsReader the XML definitions reader
   * @throws LaserDefinitionXMLException if the XML unmarshalling failed
   * @throws LaserDefinitionNotValidException if the definition failed validation
   * @throws LaserDefinitionNotFoundException if the definition was not found
   * @throws LaserDefinitionNotAllowedException if the user is not allowed
   * @throws LaserDefinitionException if the request can not be served
   */
  public void uploadMask(Reader xmlDefinitionsReader) throws LaserDefinitionException;
}

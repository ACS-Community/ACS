package cern.laser.definition.impl;

import java.io.Reader;
import java.io.Writer;
import java.util.Collection;

import cern.laser.business.definition.data.AlarmDefinition;
import cern.laser.business.definition.data.MaintenanceMask;
import cern.laser.business.definition.data.ModeMask;
import cern.laser.business.definition.data.MultiplicityThreshold;
import cern.laser.business.definition.data.ReductionLink;
import cern.laser.definition.LaserDefinitionException;
import cern.laser.definition.LaserDefinitionNotValidException;
import cern.laser.definition.ReductionMaskDefinitionHandler;

/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
 */
public class ReductionMaskDefinitionHandlerImpl extends DefinitionHandlerImpl implements ReductionMaskDefinitionHandler {
  /**
   * Creates a new ReductionMaskDefinitionHandlerImpl object.
   *
   * @param userId DOCUMENT ME!
   */
  public ReductionMaskDefinitionHandlerImpl(String userId) throws LaserDefinitionException {
    super(userId);
  }

  /**
   * DOCUMENT ME!
   *
   * @param threshold DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws LaserDefinitionNotValidException DOCUMENT ME!
   */
  public void setMultiplicityThreshold(MultiplicityThreshold threshold) throws LaserDefinitionException {
    throw new UnsupportedOperationException();
  }

  /**
   * DOCUMENT ME!
   *
   * @param alarm DOCUMENT ME!
   * @param maintenance DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   */
  public void addMaintenanceMask(AlarmDefinition alarm, MaintenanceMask maintenance) throws LaserDefinitionException {
    System.out.println("TBD");
  }

  /**
   * DOCUMENT ME!
   *
   * @param alarm DOCUMENT ME!
   * @param mode DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   */
  public void addModeMask(AlarmDefinition alarm, ModeMask mode) throws LaserDefinitionException {
    System.out.println("TBD");
  }

  /**
   * DOCUMENT ME!
   *
   * @param link DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws LaserDefinitionNotValidException DOCUMENT ME!
   */
  public void createMultiplicityLink(ReductionLink link) throws LaserDefinitionException {
    throw new UnsupportedOperationException();
  }

  /**
   * DOCUMENT ME!
   *
   * @param link DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws LaserDefinitionNotValidException DOCUMENT ME!
   */
  public void createNodeLink(ReductionLink link) throws LaserDefinitionException {
    throw new UnsupportedOperationException();
  }

  /**
   * DOCUMENT ME!
   *
   * @param xmlDefinitionsWriter DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   */
  public void download(Writer xmlDefinitionsWriter) throws LaserDefinitionException {
    System.out.println("TBD");
  }

  /**
   * DOCUMENT ME!
   *
   * @param alarm DOCUMENT ME!
   * @param maintenance DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   */
  public void removeMaintenanceMask(AlarmDefinition alarm, MaintenanceMask maintenance) throws LaserDefinitionException {
    System.out.println("TBD");
  }

  /**
   * DOCUMENT ME!
   *
   * @param alarm DOCUMENT ME!
   * @param mode DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   */
  public void removeModeMask(AlarmDefinition alarm, ModeMask mode) throws LaserDefinitionException {
    System.out.println("TBD");
  }

  /**
   * DOCUMENT ME!
   *
   * @param link DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws LaserDefinitionNotValidException DOCUMENT ME!
   */
  public void removeMultiplicityLink(ReductionLink link) throws LaserDefinitionException {
    throw new UnsupportedOperationException();
  }

  /**
   * DOCUMENT ME!
   *
   * @param link DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws LaserDefinitionNotValidException DOCUMENT ME!
   */
  public void removeNodeLink(ReductionLink link) throws LaserDefinitionException {
    throw new UnsupportedOperationException();
  }

  /**
   * DOCUMENT ME!
   *
   * @param toBeCreated DOCUMENT ME!
   * @param toBeRemoved DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   */
  public void uploadNode(Collection toBeCreated, Collection toBeRemoved) throws LaserDefinitionException {
    throw new UnsupportedOperationException();
  }

  public void uploadMultiplicity(Collection toBeCreated, Collection toBeRemoved, Collection thresholds) throws LaserDefinitionException {
    throw new UnsupportedOperationException();
  }

  public void uploadMaintenance(Collection toBeCreated, Collection toBeRemoved) throws LaserDefinitionException {
    System.out.println("TBD");
  }

  public void uploadMode(Collection toBeCreated, Collection toBeRemoved) throws LaserDefinitionException {
    System.out.println("TBD");
  }

  /**
   * DOCUMENT ME!
   *
   * @param xmlDefinitionsReader DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   */
  public void uploadReduction(Reader xmlDefinitionsReader) throws LaserDefinitionException {
    throw new UnsupportedOperationException();
  }

  public void uploadMask(Reader xmlDefinitionsReader) throws LaserDefinitionException {
    System.out.println("TBD");
  }
}

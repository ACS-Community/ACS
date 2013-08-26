package cern.laser.definition.impl;

import java.io.Reader;

import java.io.Writer;
import java.util.Collection;

import org.omg.CORBA.ORB;

import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogger;
import alma.alarmsystem.AlarmService;

import cern.laser.business.definition.data.SourceDefinition;
import cern.laser.client.impl.common.AlarmServiceSingleton;
import cern.laser.definition.LaserDefinitionDuplicationException;
import cern.laser.definition.LaserDefinitionException;
import cern.laser.definition.LaserDefinitionNotAllowedException;
import cern.laser.definition.LaserDefinitionNotFoundException;
import cern.laser.definition.LaserDefinitionNotValidException;
import cern.laser.definition.LaserDefinitionXMLException;
import cern.laser.definition.SourceDefinitionHandler;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.7 $
 */
public class SourceDefinitionHandlerImpl extends DefinitionHandlerImpl implements SourceDefinitionHandler {
	private AlarmService alarmService;
	
	/**
   * Creates a new SourceDefinitionHandlerImpl object.
   *
   * @param userId DOCUMENT ME!
   */
  public SourceDefinitionHandlerImpl(String userId, ORB orb, AcsLogger logger) throws LaserDefinitionException {
    super(userId);
    try {
		  this.alarmService=AlarmServiceSingleton.getInstance(orb,logger);
	  } catch (Throwable t) {
		  throw new LaserDefinitionException("Error getting the alarm service",t);
	  }
  }

  /**
   * DOCUMENT ME!
   *
   * @param definition DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws LaserDefinitionNotValidException DOCUMENT ME!
   * @throws LaserDefinitionNotAllowedException DOCUMENT ME!
   * @throws LaserDefinitionDuplicationException DOCUMENT ME!
   */
  public void createSource(SourceDefinition definition) throws LaserDefinitionException {
//	  alarmService.createSource(getUserId(), definition);
	  throw new UnsupportedOperationException();
  }

  /**
   * DOCUMENT ME!
   *
   * @param definitions DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws LaserDefinitionNotValidException DOCUMENT ME!
   * @throws LaserDefinitionNotAllowedException DOCUMENT ME!
   * @throws LaserDefinitionDuplicationException DOCUMENT ME!
   */
  public void createSources(Collection definitions) throws LaserDefinitionException {
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
    throw new UnsupportedOperationException();
  }

  /**
   * DOCUMENT ME!
   *
   * @param definition DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws LaserDefinitionNotFoundException DOCUMENT ME!
   * @throws LaserDefinitionNotAllowedException DOCUMENT ME!
   */
  public void removeSource(SourceDefinition definition) throws LaserDefinitionException {
    throw new UnsupportedOperationException();
  }

  /**
   * DOCUMENT ME!
   *
   * @param definition DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws LaserDefinitionNotValidException DOCUMENT ME!
   * @throws LaserDefinitionNotFoundException DOCUMENT ME!
   * @throws LaserDefinitionNotAllowedException DOCUMENT ME!
   */
  public void updateSource(SourceDefinition definition) throws LaserDefinitionException {
    throw new UnsupportedOperationException();
  }

  /**
   * DOCUMENT ME!
   *
   * @param toBeCreated DOCUMENT ME!
   * @param toBeUpdated DOCUMENT ME!
   * @param toBeRemoved DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws LaserDefinitionDuplicationException DOCUMENT ME!
   * @throws LaserDefinitionNotFoundException DOCUMENT ME!
   * @throws LaserDefinitionNotValidException DOCUMENT ME!
   * @throws LaserDefinitionNotAllowedException DOCUMENT ME!
   */
  public void upload(Collection toBeCreated, Collection toBeUpdated, Collection toBeRemoved) throws LaserDefinitionException {
    throw new UnsupportedOperationException();
  }

  /**
   * DOCUMENT ME!
   *
   * @param xmlDefinitionsReader DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws LaserDefinitionXMLException DOCUMENT ME!
   */
  public void upload(Reader xmlDefinitionsReader) throws LaserDefinitionException {
    throw new UnsupportedOperationException();
  }
}

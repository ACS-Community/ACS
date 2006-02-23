package cern.laser.definition.impl;

import java.io.Reader;

import java.io.Writer;
import java.util.Collection;

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
 * @version $Revision: 1.3 $
 */
public class SourceDefinitionHandlerImpl extends DefinitionHandlerImpl implements SourceDefinitionHandler {
	private AlarmService alarmService;
	
	/**
   * Creates a new SourceDefinitionHandlerImpl object.
   *
   * @param userId DOCUMENT ME!
   */
  public SourceDefinitionHandlerImpl(String userId) throws LaserDefinitionException {
    super(userId);
    alarmService = AlarmServiceSingleton.getInstance();
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

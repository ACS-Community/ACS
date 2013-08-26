package cern.laser.definition.impl;

import java.io.Reader;
import java.io.Writer;
import java.util.Collection;

import org.omg.CORBA.ORB;

import cern.laser.business.definition.data.CategoryDefinition;
import cern.laser.client.impl.common.AlarmServiceSingleton;
import cern.laser.definition.CategoryDefinitionHandler;
import cern.laser.definition.LaserDefinitionDuplicationException;
import cern.laser.definition.LaserDefinitionException;
import cern.laser.definition.LaserDefinitionNotAllowedException;
import cern.laser.definition.LaserDefinitionNotFoundException;
import cern.laser.definition.LaserDefinitionNotValidException;
import cern.laser.definition.LaserDefinitionXMLException;

import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogger;
import alma.alarmsystem.AlarmService;

/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.6 $
 */
public class CategoryDefinitionHandlerImpl extends DefinitionHandlerImpl implements CategoryDefinitionHandler {
	private AlarmService alarmService;
	
	/**
   * Creates a new CategoryDefinitionHandlerImpl object.
   *
   * @param userId DOCUMENT ME!
   */
  public CategoryDefinitionHandlerImpl(String userId, ORB orb, AcsLogger logger) throws LaserDefinitionException {
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
   * @param definitions DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws LaserDefinitionNotValidException DOCUMENT ME!
   * @throws LaserDefinitionDuplicationException DOCUMENT ME!
   * @throws LaserDefinitionNotAllowedException DOCUMENT ME!
   */
  public void createCategories(Collection definitions) throws LaserDefinitionException {
    throw new UnsupportedOperationException();
  }

  /**
   * DOCUMENT ME!
   *
   * @param definition DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws LaserDefinitionNotValidException DOCUMENT ME!
   * @throws LaserDefinitionDuplicationException DOCUMENT ME!
   * @throws LaserDefinitionNotAllowedException DOCUMENT ME!
   */
  public void createCategory(CategoryDefinition definition) throws LaserDefinitionException {
  //  alarmService.createCategory(getUserId(), definition);
  }

  /**
   * DOCUMENT ME!
   *
   * @param xmlDefinitionsWriter DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws LaserDefinitionNotAllowedException DOCUMENT ME!
   * @throws LaserDefinitionXMLException DOCUMENT ME!
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
  public void removeCategory(CategoryDefinition definition) throws LaserDefinitionException {
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
  public void updateCategory(CategoryDefinition definition) throws LaserDefinitionException {
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

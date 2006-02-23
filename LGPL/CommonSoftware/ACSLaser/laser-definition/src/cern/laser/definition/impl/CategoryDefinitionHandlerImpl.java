package cern.laser.definition.impl;

import java.io.Reader;
import java.io.Writer;
import java.util.Collection;

import cern.laser.business.definition.data.CategoryDefinition;
import cern.laser.client.impl.common.AlarmServiceSingleton;
import cern.laser.definition.CategoryDefinitionHandler;
import cern.laser.definition.LaserDefinitionDuplicationException;
import cern.laser.definition.LaserDefinitionException;
import cern.laser.definition.LaserDefinitionNotAllowedException;
import cern.laser.definition.LaserDefinitionNotFoundException;
import cern.laser.definition.LaserDefinitionNotValidException;
import cern.laser.definition.LaserDefinitionXMLException;

import alma.alarmsystem.AlarmService;

/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
 */
public class CategoryDefinitionHandlerImpl extends DefinitionHandlerImpl implements CategoryDefinitionHandler {
	private AlarmService alarmService;
	
	/**
   * Creates a new CategoryDefinitionHandlerImpl object.
   *
   * @param userId DOCUMENT ME!
   */
  public CategoryDefinitionHandlerImpl(String userId) throws LaserDefinitionException {
    super(userId);
    alarmService = AlarmServiceSingleton.getInstance();
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

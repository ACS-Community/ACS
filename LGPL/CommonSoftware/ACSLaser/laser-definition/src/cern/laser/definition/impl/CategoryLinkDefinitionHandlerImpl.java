package cern.laser.definition.impl;

import java.io.Reader;
import java.io.Writer;
import java.util.Collection;

import cern.laser.business.definition.data.CategoryLink;
import cern.laser.definition.CategoryLinkDefinitionHandler;
import cern.laser.definition.LaserDefinitionException;
import cern.laser.definition.LaserDefinitionNotAllowedException;
import cern.laser.definition.LaserDefinitionNotFoundException;
import cern.laser.definition.LaserDefinitionNotValidException;
import cern.laser.definition.LaserDefinitionXMLException;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
 */
public class CategoryLinkDefinitionHandlerImpl extends DefinitionHandlerImpl implements CategoryLinkDefinitionHandler {
  /**
* Creates a new CategoryLinkDefinitionHandlerImpl object.
*
* @param userId DOCUMENT ME!
*/
  public CategoryLinkDefinitionHandlerImpl(String userId) throws LaserDefinitionException {
    super(userId);
  }

  /**
* DOCUMENT ME!
*
* @param link DOCUMENT ME!
*
* @throws LaserDefinitionException DOCUMENT ME!
* @throws LaserDefinitionNotValidException DOCUMENT ME!
* @throws LaserDefinitionNotFoundException DOCUMENT ME!
* @throws LaserDefinitionNotAllowedException DOCUMENT ME!
*/
  public void createCategoryLink(CategoryLink link) throws LaserDefinitionException {
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
* @param link DOCUMENT ME!
*
* @throws LaserDefinitionException DOCUMENT ME!
* @throws LaserDefinitionNotValidException DOCUMENT ME!
* @throws LaserDefinitionNotFoundException DOCUMENT ME!
* @throws LaserDefinitionNotAllowedException DOCUMENT ME!
*/
  public void removeCategoryLink(CategoryLink link) throws LaserDefinitionException {
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
  public void upload(Collection toBeCreated, Collection toBeRemoved) throws LaserDefinitionException {
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

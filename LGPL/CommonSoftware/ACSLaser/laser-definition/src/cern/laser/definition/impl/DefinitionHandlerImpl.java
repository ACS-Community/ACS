package cern.laser.definition.impl;

import cern.laser.definition.LaserDefinitionException;

/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.3 $
 */
public class DefinitionHandlerImpl {
  protected static final String DEFAULT_XSD_LOCATION = "http://proj-laser.web.cern.ch/proj-laser/xml/laser-definition.xsd";
  protected static final String XSD_LOCATION_PROPERTY = "laser.definition.xsd";
  private String userId;

  /**
   * Creates a new DefinitionHandlerImpl object.
   *
   * @param userId DOCUMENT ME!
   */
  public DefinitionHandlerImpl(String userId) throws LaserDefinitionException {
    try {
      setUserId(userId);
    } catch (Exception e) 
    {
      throw new LaserDefinitionException(e.getMessage(), e);
    }
  }

  protected void setUserId(String newUserId) {
    userId = newUserId;
  }

  protected String getUserId() {
    return userId;
  }
}

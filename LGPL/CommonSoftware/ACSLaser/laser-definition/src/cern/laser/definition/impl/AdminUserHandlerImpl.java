package cern.laser.definition.impl;

import java.util.Collection;

import cern.laser.client.LaserConnectionException;
import cern.laser.client.LaserException;
import cern.laser.client.impl.common.AlarmServiceSingleton;
import alma.acs.container.ContainerServicesBase;
import alma.alarmsystem.CERNAlarmService;
import cern.laser.definition.AdminUser;
import cern.laser.definition.AdminUserHandler;
import cern.laser.definition.LaserDefinitionDuplicationException;
import cern.laser.definition.LaserDefinitionException;
import cern.laser.definition.LaserDefinitionLoginException;
import cern.laser.definition.LaserDefinitionNotFoundException;

/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.6 $
 */
public class AdminUserHandlerImpl extends AdminUserHandler {
  private CERNAlarmService alarmService;

  public AdminUserHandlerImpl(ContainerServicesBase contSvcs) throws LaserException, LaserConnectionException
  {
    super();
    try {
		  this.alarmService=AlarmServiceSingleton.getInstance(contSvcs);
	  } catch (Throwable t) {
		  throw new LaserConnectionException("Error getting the alarm service",t);
	  }
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   */
  public Collection getUsers() throws LaserDefinitionException {
    throw new UnsupportedOperationException(); /*try {
      Collection user_ids = getDefinitionServiceSessionEJB().getAdminUsers();
      Collection users = new ArrayList();
      Iterator iterator = user_ids.iterator();

      while (iterator.hasNext()) {
        users.add(new AdminUserImpl((String) iterator.next()));
      }

      return users;
    } catch (Exception e) {
      throw new LaserDefinitionException("unable to get users : " + e.getMessage(), e);
    }*/
  }

  /**
   * DOCUMENT ME!
   *
   * @param name DOCUMENT ME!
   * @param password DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws IllegalArgumentException DOCUMENT ME!
   * @throws LaserDefinitionDuplicationException DOCUMENT ME!
   */
  public AdminUser createUser(String name, String password) throws LaserDefinitionException {
  	if ((name == null) || (password == null)) {
      throw new IllegalArgumentException("argument can not be null");
    }
  	
    try {
    // TODO
    	String id = alarmService.createAdminUser(name, password);
      return new AdminUserImpl(id);
  //  } catch (cern.laser.business.definition.LaserDefinitionDuplicationException de) {
   //   throw new LaserDefinitionDuplicationException("user already defined : " + name, de);
    } catch (Exception e) {
      throw new LaserDefinitionException("unable to create user : " + name, e);
    }
  }

  /**
   * DOCUMENT ME!
   *
   * @param name DOCUMENT ME!
   * @param password DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws IllegalArgumentException DOCUMENT ME!
   * @throws LaserDefinitionLoginException DOCUMENT ME!
   */
  public AdminUser loginUser(String name, String password) throws LaserDefinitionException {
  	if ((name == null) || (password == null)) {
      throw new IllegalArgumentException("argument can not be null");
    }

    try {
      return new AdminUserImpl(alarmService.loginAdminUser(name, password));
    } catch (Exception e) {
      throw new LaserDefinitionException("unable to login user " + name, e);
    }
  }

  /**
   * DOCUMENT ME!
   *
   * @param name DOCUMENT ME!
   *
   * @throws LaserDefinitionException DOCUMENT ME!
   * @throws IllegalArgumentException DOCUMENT ME!
   * @throws LaserDefinitionNotFoundException DOCUMENT ME!
   */
  public void removeUser(String name) throws LaserDefinitionException {
  	throw new UnsupportedOperationException(); /*if (name == null) {
      throw new IllegalArgumentException("argument can not be null");
    }

    try {
      getDefinitionServiceSessionEJB().removeAdminUser(name);
    } catch (cern.laser.business.definition.LaserDefinitionNotFoundException ne) {
      throw new LaserDefinitionNotFoundException("user not found : " + name, ne);
    } catch (Exception e) {
      throw new LaserDefinitionException("unable to remove user : " + name, e);
    }*/
  }

  private Object getDefinitionServiceSessionEJB() throws Exception {
  	throw new UnsupportedOperationException(); /*if (definition == null) {
      DefinitionServiceSessionEJBHome definition_home = null;
      Enumeration contexts = laser.getLaserContexts();
      StringBuffer errors = new StringBuffer();
      while (contexts.hasMoreElements()) {
        try {
          Context context = (Context)contexts.nextElement();
          definition_home = (DefinitionServiceSessionEJBHome)PortableRemoteObject.narrow(context.lookup("DefinitionServiceSessionEJB"), DefinitionServiceSessionEJBHome.class);
          definition = definition_home.create();

          return definition;
        } catch (Exception e) {
          errors.append("\n[");
          errors.append(e.getMessage());
          errors.append("]");
        }
      }
      throw new LaserConnectionException("unable to connect to the LASER definition service : " + errors.toString());
    }

    return definition;*/
  }

}

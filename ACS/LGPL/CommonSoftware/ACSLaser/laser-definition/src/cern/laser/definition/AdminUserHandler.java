package cern.laser.definition;

import java.util.Collection;

import org.omg.CORBA.ORB;

import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogger;

import cern.laser.client.LaserConnectionException;
import cern.laser.client.LaserException;
import cern.laser.definition.impl.AdminUserHandlerImpl;


/** Provides the service to handle adminitrative users.
 * @see cern.laser.definition.AdminUser
 */
public abstract class AdminUserHandler {
  private static AdminUserHandler instance = null;

  /** Factory method.
   * @return an instance of the implementation class
   * @throws LaserException
   * @throws LaserConnectionException
   */
  public static AdminUserHandler get(ORB orb, AcsLogger logger) throws LaserException, LaserConnectionException {
    if (instance == null) {
      instance = new AdminUserHandlerImpl(orb,logger);
    }

    return instance;
  }

  /** Get the administrative users.
   * @throws LaserDefinitionException if the request can not be served
   * @return the defined administrative users
   */
  public abstract Collection getUsers() throws LaserDefinitionException;

  /** Create a new administrative user.
   * @throws LaserDefinitionDuplicationException if an administrative user with the same name already exists
   * @throws LaserDefinitionException if the request can not be served
   * @return the administrative user
   */
  public abstract AdminUser createUser(String name, String password) throws LaserDefinitionException;

  /** Authenticate an administrative user.
   * @throws LaserDefinitionNotAllowedException if the user can not be authenticated
   * @throws LaserDefinitionException if the request can not be served
   * @return the administrative user
   */
  public abstract AdminUser loginUser(String name, String password) throws LaserDefinitionException;

  /** Remove an administrative user.
   * @throws LaserDefinitionNotFoundException if the user does not exist
   * @throws LaserDefinitionException if the request can not be served
   */
  public abstract void removeUser(String name) throws LaserDefinitionException;
}

/*
 * $Id: UserHandlerImpl.java,v 1.6 2011/04/13 15:45:42 acaproni Exp $
 *
 * $Date: 2011/04/13 15:45:42 $ 
 * $Revision: 1.6 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.console.impl;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;

import javax.naming.Context;
import javax.rmi.PortableRemoteObject;

import org.omg.CORBA.ORB;

import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogger;

import cern.laser.client.LaserConnectionException;
import cern.laser.console.LaserConsoleException;
import cern.laser.console.LaserUserDuplicationException;
import cern.laser.console.LaserUserNotFoundException;
import cern.laser.console.User;
import cern.laser.console.UserHandler;

public class UserHandlerImpl extends UserHandler
{  
	// The user
	private UserImpl user;
	// The list of the users
	private ArrayList users = new ArrayList();
	
  public UserHandlerImpl(ORB orb, AcsLogger logger) throws LaserConsoleException 
  {
	  // Create a default user: test
	  user = new UserImpl("test",orb,logger);
	  // Add the user to the collection
	  users.add(user);
  }

  public Collection getUsers() throws LaserConsoleException 
  {
	  return users;
  	//throw new UnsupportedOperationException();
  	/* try {
      Collection users_ejb = null;
      try 
      {
        users_ejb = userHome.findAll();
      } catch (Exception e) 
      {
        userHome = getConsoleUserEntityEJBHome();
        users_ejb = userHome.findAll();
      }
      Collection users = new ArrayList();
      Iterator iterator = users_ejb.iterator();
      while (iterator.hasNext()) 
      {
        users.add(new UserImpl((ConsoleUserEntityEJB)iterator.next()));
      }

      return users;
    } catch (Exception e) 
    {
      throw new LaserConsoleException("unable to get defined users : " + e.getMessage(), e);
    }*/
  }

  public User getUser(String name, ORB orb, AcsLogger logger) throws LaserConsoleException 
  {
	  // Check if the user is already in the list
	  boolean found=false;
	  int t;
	  for (t=0; t<users.size() && !found; t++) {
		  if (((UserImpl)users.get(t)).getName().equalsIgnoreCase(name)) {
			  user = (UserImpl)users.get(t);
			  return user;
		  }
	  }
	  user = new UserImpl(name,orb,logger);
	  users.add(user);
	  return user;
  	
  	//throw new UnsupportedOperationException();
  	/* User result = null;
    try {
      try {
        result = new UserImpl(userHome.findByName(name));
      } catch (Exception e) 
      {
        userHome = getConsoleUserEntityEJBHome();
        result = new UserImpl(userHome.findByName(name));
      }
    } catch (ObjectNotFoundException onfe) 
    {
      throw new LaserUserNotFoundException("unable to get user : " + onfe.getMessage(), onfe);
    } catch (Exception e)
    {
      throw new LaserConsoleException("unable to get user : " + e.getMessage(), e);
    }

    return result;*/
  }

  public User createUser(String name, String password) throws LaserConsoleException
  {
  	throw new UnsupportedOperationException();/*  ConsoleUserEntityEJB user = null;
    try 
    {
      try {
        userHome.findByName(name);
      } catch (Exception e) 
      {
        userHome = getConsoleUserEntityEJBHome();
        userHome.findByName(name);
      }
    } catch (ObjectNotFoundException onf) {
      try {
        user = userHome.create(name, password);
      
        return new UserImpl(user);
      } catch (Exception e) {
        throw new LaserConsoleException("unable to create a new user : " + e.getMessage(), e);
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to create a new user : " + e.getMessage(), e);
    }
    throw new LaserUserDuplicationException("user " + name + " is already defined");*/
  }


  public void removeUser(String name) throws LaserConsoleException 
  {
  	throw new UnsupportedOperationException();/* try {
      ConsoleUserEntityEJB user = null;
      try {
        user = userHome.findByName(name);
      } catch (Exception e) 
      {
        userHome = getConsoleUserEntityEJBHome();
        userHome.findByName(name);
      }
      user.remove();
    } catch (ObjectNotFoundException onfe) 
    {
      new LaserUserNotFoundException("unable to get user : " + onfe.getMessage(), onfe);
    } catch (Exception e) 
    {
      throw new LaserConsoleException("unable to remove the user : " + e.getMessage(), e);
    }*/
  }

  private Object getConsoleUserEntityEJBHome() throws LaserConnectionException 
  {
  	throw new UnsupportedOperationException();/*  ConsoleUserEntityEJBHome home = null;
    Enumeration contexts = laser.getLaserContexts();
    StringBuffer errors = new StringBuffer();
    while (contexts.hasMoreElements()) {
      try {
        Context context = (Context)contexts.nextElement();
        home = (ConsoleUserEntityEJBHome)PortableRemoteObject.narrow(context.lookup("ConsoleUserEntityEJB"), ConsoleUserEntityEJBHome.class);
        
        return home;
      } catch (Exception e) {
        errors.append("\n[");
        errors.append(e.getMessage());
        errors.append("]");
      }
    }
    throw new LaserConnectionException("unable to connect to LASER : " + errors.toString());*/
  }

}

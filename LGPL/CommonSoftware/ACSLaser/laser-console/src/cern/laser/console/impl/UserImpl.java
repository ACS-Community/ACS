/*
 * $Id: UserImpl.java,v 1.9 2011/04/13 15:45:42 acaproni Exp $
 *
 * $Date: 2011/04/13 15:45:42 $ 
 * $Revision: 1.9 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.console.impl;

import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogger;
import alma.alarmsystem.AlarmService;
import cern.laser.client.LaserConnectionException;
import cern.laser.client.data.Category;
import cern.laser.client.impl.common.AlarmServiceSingleton;
import cern.laser.client.impl.services.selection.AlarmSelectionHandlerImpl;
import cern.laser.client.services.selection.Selection;
import cern.laser.console.Behaviour;
import cern.laser.console.CommentedAlarmMap;
import cern.laser.console.Configuration;
import cern.laser.console.LaserConsoleException;
import cern.laser.console.LaserUserNotFoundException;
import cern.laser.console.User;
import cern.laser.client.impl.services.selection.CategorySelectionImpl;

import java.util.ArrayList;

import java.util.Collection;

import org.omg.CORBA.ORB;

public class UserImpl implements User {
//  private String userId;
  private AlarmService laser;
  
  private ArrayList configurations = new ArrayList();

  // TODO this is temporary... to be replacedwith real impl on ConsoleUserEntityEJB
  private class LocalConsoleUser {
  	public String userId;
  	public String password;
  	
  	public LocalConsoleUser(String userId, String password)
  	{
  		this.userId = userId;
  		this.password = password;
  	}
  }
  private LocalConsoleUser user;
  
  /**
   * The ORB
   */
  private final ORB orb;
  
  /**
   * The logger
   */
  private final AcsLogger logger;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

//  public UserImpl(Object newUser) throws LaserConsoleException {
  public UserImpl(String newUserName, ORB orb, AcsLogger logger) throws LaserConsoleException {
	  this.user = new LocalConsoleUser(newUserName, newUserName);
	  this.orb=orb;
	  this.logger=logger;
      try {
    	  this.laser = AlarmServiceSingleton.getInstance(orb,logger);
      } catch (Exception e) {
    	  throw new LaserConsoleException("unable to create a console user : " + e.getMessage(), e);
      }
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  //
  // -- implements User ---------------------------------------------
  //

  public String getName() throws LaserConsoleException {
	  return this.user.userId;
  }

  public void setName(String newName) throws LaserConsoleException {
      this.user.userId = newName;
	  
//  	throw new UnsupportedOperationException();
/*      try {
        user.setUserId(newName);
      } catch (Exception e) {
        user = getConsoleUserEntityEJB();
        user.setUserId(newName);
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to set user name : " + e.getMessage(), e);
    }
*/  }

  public String getPassword() throws LaserConsoleException {
	  if (this.user.password == null) {
		  throw new IllegalArgumentException("User's password not set!");
	  }
	  return this.user.password;
//  	throw new UnsupportedOperationException();
/*	  try {
        return user.getPassword();
      } catch (Exception e) {
        user = getConsoleUserEntityEJB();
        return user.getPassword();
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to get user password : " + e.getMessage(), e);
    }*/
  }

  public void setPassword(String newPassword) throws LaserConsoleException {
	  if (newPassword == null) {
		  throw new IllegalArgumentException("Null given to set as user's password!");
	  }
	  this.user.password = newPassword;
//  	throw new UnsupportedOperationException();
  	/* try {
        user.setPassword(newPassword);
      } catch (Exception e) {
        user = getConsoleUserEntityEJB();
        user.setPassword(newPassword);
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to set user password : " + e.getMessage(), e);
    }*/
  }

  public String getDefaultPrinter() throws LaserConsoleException {
  	throw new UnsupportedOperationException();/* try {
      try {
        return user.getDefaultPrinter();
      } catch (Exception e) {
        user = getConsoleUserEntityEJB();
        return user.getDefaultPrinter();
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to get user default printer : " + e.getMessage(), e);
    }*/
  }

  public void setDefaultPrinter(String newDefaultPrinter) throws LaserConsoleException {
  	throw new UnsupportedOperationException();/* try {
      try {
        user.setDefaultPrinter(newDefaultPrinter);
      } catch (Exception e) {
        user = getConsoleUserEntityEJB();
        user.setDefaultPrinter(newDefaultPrinter);
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to set user default printer : " + e.getMessage(), e);
    }*/
  }

  public Collection getConfigurations() throws LaserConsoleException, LaserConnectionException {
	if (configurations.size()==0) {
		configurations.add(getDefaultConfiguration());
	}
	return configurations;
  	//throw new UnsupportedOperationException();
	/* try {
      Collection configurations_ejb = null;
      try {
        configurations_ejb = user.getRemoteConfigurations();
      } catch (Exception e) {
        user = getConsoleUserEntityEJB();
        configurations_ejb = user.getRemoteConfigurations();
      }
      Collection configurations = new ArrayList();
      Iterator iterator = configurations_ejb.iterator();
      while (iterator.hasNext()) {
        configurations.add(new ConfigurationImpl((ConsoleConfigurationEntityEJB) iterator.next()));
      }

      return configurations;
    } catch (RemoteException e) {
      throw new LaserConsoleException("unable to get user configurations : " + e.getMessage(), e);
    } catch (LaserException e) {
      throw new LaserConsoleException("unable to get user configurations : " + e.getMessage(), e);
    }*/
  }

  public Configuration getConfiguration(String name) throws LaserConsoleException, LaserConnectionException {
  	return new ConfigurationImpl(name,orb,logger);
  	/* try {
      try {
        return new ConfigurationImpl(user.getRemoteConfiguration(name));
      } catch (Exception e) {
        user = getConsoleUserEntityEJB();
        return new ConfigurationImpl(user.getRemoteConfiguration(name));
      }
    } catch (ObjectNotFoundException onf) {
      throw new LaserConfigurationNotFoundException("unable to get user configuration : " + onf.getMessage(), onf);
    } catch (RemoteException e) {
      throw new LaserConsoleException("unable to get user configuration : " + e.getMessage(), e);
    }*/
  }

  public Configuration createConfiguration(String name) throws LaserConsoleException {
	  Configuration config;
	  try {
		  config = getDefaultConfiguration();
	  } catch (Exception e) {
		  e.printStackTrace();
		  throw new LaserConsoleException(e.getMessage());
	  }
	  CategorySelectionImpl catSel = new  CategorySelectionImpl();
	  /*Category categoryRoot;
	  try {
		  categoryRoot =
			  CategoryBrowsingHandlerFactory.getHandler().getCategoryTreeRoot();
	  } catch (Exception e) {
		  System.out.println("# UserImpl::createConfiguration exception "+e.getMessage());
		  e.printStackTrace();
		  throw new LaserConsoleException(e.getMessage());
	  }
	  catSel.addAll(CategoriesPreLoader.getInstance().getChildren(categoryRoot));*/
	  config.getSelection().setCategorySelection(catSel);
	  
	  return config;
  	//throw new UnsupportedOperationException();
	  /* try {
      try {
        configurationHome.findByNameAndUserId(name, userId);
      } catch (Exception e) {
        configurationHome = getConsoleConfigurationEntityEJBHome();
        configurationHome.findByNameAndUserId(name, userId);
      }
    } catch (ObjectNotFoundException onf) {
      try {
        ConsoleConfigurationEntityEJB configuration_ejb = null;
        try {
          configuration_ejb = user.createRemoteConfiguration(name);
        } catch (Exception e) {
          user = getConsoleUserEntityEJB();
          configuration_ejb = user.createRemoteConfiguration(name);
        }
        Configuration result = new ConfigurationImpl(configuration_ejb);
        result.setBehaviour(result.createBehaviour());
        result.setSelection(AlarmSelectionHandler.get().createSelection());
        result.setHighlighted(new CommentedAlarmMap());
        result.setAutoHighlighted(new CommentedAlarmMap());
        result.setAutoKlaxoned(new CommentedAlarmMap());
        result.setInhibited(new CommentedAlarmMap());
        result.setMasked(new CommentedAlarmMap());

        return result;
      } catch (Exception e) {
        throw new LaserConsoleException("unable to create user configuration : " + e.getMessage(), e);
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to create user configuration : " + e.getMessage(), e);
    }
    throw new LaserConfigurationDuplicationException("configuration " + name + " is already defined");*/
  }

  public Configuration createConfiguration(Configuration configuration) throws LaserConsoleException {
  	throw new UnsupportedOperationException();/*  try {
      try {
        configurationHome.findByNameAndUserId(configuration.getName(), userId);
      } catch (Exception e) {
        configurationHome = getConsoleConfigurationEntityEJBHome();
        configurationHome.findByNameAndUserId(configuration.getName(), userId);
      }
    } catch (ObjectNotFoundException onf) {
      try {
        ConsoleConfigurationEntityEJB configuration_ejb = null;
        try {
          configuration_ejb = user.createRemoteConfiguration(configuration.getName());
        } catch (Exception e) {
          user = getConsoleUserEntityEJB();
          configuration_ejb = user.createRemoteConfiguration(configuration.getName());
        }
        Configuration result = new ConfigurationImpl(configuration_ejb);
        result.setBehaviour(configuration.getBehaviour());
        result.setSelection(configuration.getSelection());
        result.setHighlighted(configuration.getHighlighted());
        result.setAutoHighlighted(configuration.getAutoHighlighted());
        result.setAutoKlaxoned(configuration.getAutoKlaxoned());
        result.setInhibited(configuration.getInhibited());
        result.setMasked(configuration.getMasked());

        return result;
      } catch (Exception e) {
        throw new LaserConsoleException("unable to create user configuration : " + e.getMessage(), e);
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to create user configuration : " + e.getMessage(), e);
    }
    throw new LaserConfigurationDuplicationException("configuration " + configuration.getName() + " is already defined");*/
  }

  public void removeConfiguration(String name) throws LaserConsoleException {
  	/*throw new UnsupportedOperationException(); try {
      try {
        user.removeRemoteConfiguration(name);
      } catch (Exception e) {
        user = getConsoleUserEntityEJB();
        user.removeRemoteConfiguration(name);
      }
    } catch (ObjectNotFoundException onf) {
      throw new LaserConfigurationNotFoundException("unable to remove user configuration : " + onf.getMessage(), onf);
    } catch (Exception e) {
      throw new LaserConsoleException("unable to remove user configuration : " + e.getMessage(), e);
    }*/
  }

  public Configuration getDefaultConfiguration() throws LaserConsoleException, LaserConnectionException {
  	// TODO temporary
  	try {
  	return new Configuration()  {
  		
  		private String name = "Client-side in memory configuration";
  		private boolean isDefault = true;
  		private Behaviour behaviour = new BehaviourImpl();
        private Selection selection = AlarmSelectionHandlerImpl.get(orb,logger).createSelection();
        private CommentedAlarmMap highlighted = new CommentedAlarmMap();
        private CommentedAlarmMap autoHighlighted = new CommentedAlarmMap();
        private CommentedAlarmMap autoKlaxoned = new CommentedAlarmMap();
        private CommentedAlarmMap inhibited = new CommentedAlarmMap();
        private CommentedAlarmMap masked = new CommentedAlarmMap();
        private CommentedAlarmMap acknowledged = new CommentedAlarmMap();
        private CommentedAlarmMap newIndicator = new CommentedAlarmMap();
        private Boolean activeListFont = new Boolean(false);
 		
		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#getName()
		 */
		public String getName() throws LaserConsoleException {
			return name;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#setName(java.lang.String)
		 */
		public void setName(String newName) throws LaserConsoleException {
			this.name = newName;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#isDefault()
		 */
		public boolean isDefault() throws LaserConsoleException {
			return isDefault;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#getSelection()
		 */
		public Selection getSelection() throws LaserConsoleException {
			return selection;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#setSelection(cern.laser.client.services.selection.Selection)
		 */
		public void setSelection(Selection newSelection)
				throws LaserConsoleException {
			this.selection = newSelection;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#createBehaviour()
		 */
		public Behaviour createBehaviour() {
			return new BehaviourImpl();
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#getBehaviour()
		 */
		public Behaviour getBehaviour() throws LaserConsoleException {
			return behaviour;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#setBehaviour(cern.laser.console.Behaviour)
		 */
		public void setBehaviour(Behaviour newBehaviour)
				throws LaserConsoleException {
			this.behaviour = newBehaviour;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#getMasked()
		 */
		public CommentedAlarmMap getMasked() throws LaserConsoleException {
			return masked;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#setMasked(cern.laser.console.CommentedAlarmMap)
		 */
		public void setMasked(CommentedAlarmMap newMasked)
				throws LaserConsoleException {
			this.masked = newMasked;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#getInhibited()
		 */
		public CommentedAlarmMap getInhibited() throws LaserConsoleException {
			return inhibited;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#setInhibited(cern.laser.console.CommentedAlarmMap)
		 */
		public void setInhibited(CommentedAlarmMap newInhibited)
				throws LaserConsoleException {
			this.inhibited = newInhibited;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#getHighlighted()
		 */
		public CommentedAlarmMap getHighlighted() throws LaserConsoleException {
			return highlighted;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#setHighlighted(cern.laser.console.CommentedAlarmMap)
		 */
		public void setHighlighted(CommentedAlarmMap newHighlighted)
				throws LaserConsoleException {
			this.highlighted = newHighlighted;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#getAutoHighlighted()
		 */
		public CommentedAlarmMap getAutoHighlighted()
				throws LaserConsoleException {
			return autoHighlighted;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#setAutoHighlighted(cern.laser.console.CommentedAlarmMap)
		 */
		public void setAutoHighlighted(CommentedAlarmMap newAutoHighlighted)
				throws LaserConsoleException {
			this.autoHighlighted = newAutoHighlighted;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#getAutoKlaxoned()
		 */
		public CommentedAlarmMap getAutoKlaxoned() throws LaserConsoleException {
			return autoKlaxoned;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#setAutoKlaxoned(cern.laser.console.CommentedAlarmMap)
		 */
		public void setAutoKlaxoned(CommentedAlarmMap newAutoKlaxoned)
				throws LaserConsoleException {
			this.autoKlaxoned = newAutoKlaxoned;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#getAcknowledged()
		 */
		public CommentedAlarmMap getAcknowledged() throws LaserConsoleException {
			return acknowledged;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#setAcknowledged(cern.laser.console.CommentedAlarmMap)
		 */
		public void setAcknowledged(CommentedAlarmMap newAcknowledged)
				throws LaserConsoleException {
			this.acknowledged = newAcknowledged;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#getNewIndicator()
		 */
		public CommentedAlarmMap getNewIndicator() throws LaserConsoleException {
			return newIndicator;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#setNewIndicator(cern.laser.console.CommentedAlarmMap)
		 */
		public void setNewIndicator(CommentedAlarmMap newNewIndicator)
				throws LaserConsoleException {
			this.newIndicator = newNewIndicator;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#getActiveListFont()
		 */
		public Boolean getActiveListFont() throws LaserConsoleException {
			return activeListFont;
		}

		/* (non-Javadoc)
		 * @see cern.laser.console.Configuration#setActiveListFont(java.lang.Boolean)
		 */
		public void setActiveListFont(Boolean isActiveListFont)
				throws LaserConsoleException {
			this.activeListFont = isActiveListFont;
		}
  	};
  	}
  	catch (Throwable th) {
  		throw new LaserConsoleException("Failed to create default configuration.", th);
  	}
  	//throw new UnsupportedOperationException();
  	/* try {
      ConsoleConfigurationEntityEJB configuration_ejb = null;
      try {
        configuration_ejb = user.getRemoteDefaultConfiguration();
      } catch (Exception e) {
        user = getConsoleUserEntityEJB();
        configuration_ejb = user.getRemoteDefaultConfiguration();
      }
      if (configuration_ejb == null) {
        return null;
      } else {
        return new ConfigurationImpl(configuration_ejb);
      }
    } catch (RemoteException e) {
      throw new LaserConsoleException("unable to get user configuration : " + e.getMessage(), e);
    }*/
  }

  public void setDefaultConfiguration(String name) throws LaserConsoleException {
  	throw new UnsupportedOperationException();/* try {
      try {
        user.setRemoteDefaultConfiguration(name);
      } catch (Exception e) {
        user = getConsoleUserEntityEJB();
        user.setRemoteDefaultConfiguration(name);
      }
    } catch (ObjectNotFoundException onf) {
      throw new LaserConfigurationNotFoundException("unable to set user configuration : " + onf.getMessage(), onf);
    } catch (Exception e) {
      throw new LaserConsoleException("unable to set user configuration : " + e.getMessage(), e);
    }*/
  }

  public boolean isDefaultConfiguration(String name) throws LaserConsoleException {
  	throw new UnsupportedOperationException();/* try {
      try {
        return user.isDefaultConfiguration(name);
      } catch (Exception e) {
        user = getConsoleUserEntityEJB();
        return user.isDefaultConfiguration(name);
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to check user default configuration : " + e.getMessage(), e);
    }*/
  }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

  private Object getConsoleConfigurationEntityEJBHome() throws LaserConnectionException {
  	throw new UnsupportedOperationException();/* ConsoleConfigurationEntityEJBHome home = null;
    Enumeration contexts = laser.getLaserContexts();
    StringBuffer errors = new StringBuffer();
    while (contexts.hasMoreElements()) {
      try {
        Context context = (Context) contexts.nextElement();
        home = (c) PortableRemoteObject.narrow(context
            .lookup("ConsoleConfigurationEntityEJB"), ConsoleConfigurationEntityEJBHome.class);

        return home;
      } catch (Exception e) {
        errors.append("\n[");
        errors.append(e.getMessage());
        errors.append("]");
      }
    }
    throw new LaserConnectionException("unable to connect to LASER : " + errors.toString());*/
  }

  private Object getConsoleUserEntityEJB() throws LaserConnectionException, LaserUserNotFoundException {
  	throw new UnsupportedOperationException();/* ConsoleUserEntityEJBHome home = null;
    Enumeration contexts = laser.getLaserContexts();
    StringBuffer errors = new StringBuffer();
    while (contexts.hasMoreElements()) {
      try {
        Context context = (Context) contexts.nextElement();
        home = (ConsoleUserEntityEJBHome) PortableRemoteObject.narrow(context.lookup("ConsoleUserEntityEJB"),
            ConsoleUserEntityEJBHome.class);

        return home.findByPrimaryKey(userId);
      } catch (ObjectNotFoundException onf) {
        throw new LaserUserNotFoundException("user undefined", onf);
      } catch (Exception e) {
        errors.append("\n[");
        errors.append(e.getMessage());
        errors.append("]");
      }
    }
    throw new LaserConnectionException("unable to connect to LASER : " + errors.toString());*/
  }

  public String toString() {
    StringBuffer str_buf = new StringBuffer();
    try {
      str_buf.append("\nCONSOLE USER :");
      str_buf.append("\nNAME : ");
      str_buf.append(getName());
      str_buf.append("\nPASSWORD : ");
      str_buf.append(getPassword());
    } catch (Exception e) {
      str_buf.append("exception caught : " + e.getMessage());
    }

    return str_buf.toString();
  }
}

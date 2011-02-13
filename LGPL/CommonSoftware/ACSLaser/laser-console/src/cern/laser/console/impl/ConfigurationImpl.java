/*
 * $Id: ConfigurationImpl.java,v 1.4 2011/02/13 15:37:17 acaproni Exp $
 *
 * $Date: 2011/02/13 15:37:17 $ 
 * $Revision: 1.4 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.console.impl;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;

import javax.naming.Context;
import javax.rmi.PortableRemoteObject;

import alma.acs.container.ContainerServicesBase;

import cern.laser.client.LaserConnectionException;
import cern.laser.client.data.Alarm;
import cern.laser.client.impl.services.selection.SelectionImpl;
import cern.laser.client.services.browsing.AlarmBrowsingHandler;
import cern.laser.client.services.selection.Selection;
import cern.laser.console.Behaviour;
import cern.laser.console.CommentedAlarm;
import cern.laser.console.CommentedAlarmMap;
import cern.laser.console.Configuration;
import cern.laser.console.LaserConfigurationNotFoundException;
import cern.laser.console.LaserConsoleException;

public class ConfigurationImpl implements Configuration {
 
  private Integer configurationId;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //
  
  private ContainerServicesBase contSvcs;

  public ConfigurationImpl(Object newConfiguration, ContainerServicesBase contSvcs) throws LaserConsoleException {
	  this.contSvcs=contSvcs;
  	/*try {
      laser = new LaserClientContext();
      configuration = newConfiguration;
      try {
        configurationId = configuration.getConfigurationId();
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        configurationId = configuration.getConfigurationId();
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to create a console configuration : " + e.getMessage(), e);
    }*/
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  public String toString() {
    StringBuffer str_buf = new StringBuffer();
    try {
      str_buf.append("\nCONSOLE CONFIGURATION :");
      str_buf.append("\nNAME : ");
      str_buf.append(getName());
      str_buf.append("\nSELECTION : ");
      str_buf.append(getSelection());
      str_buf.append("\nBEHAVIOUR : ");
      str_buf.append(getBehaviour());
      str_buf.append("\nMASKED : ");
      str_buf.append(getMasked());
      str_buf.append("\nINHIBITED : ");
      str_buf.append(getInhibited());
      str_buf.append("\nHIGHLIGHTED : ");
      str_buf.append(getHighlighted());
      str_buf.append("\nAUTO HIGHLIGHTED : ");
      str_buf.append(getAutoHighlighted());
      str_buf.append("\nAUTO KLAXONED : ");
      str_buf.append(getAutoKlaxoned());
    } catch (Exception e) {
      str_buf.append("exception caught : " + e.getMessage());
    }

    return str_buf.toString();
  }

  //
  // -- implements Configuration ------------------------------------
  //

  public String getName() throws LaserConsoleException {
  	throw new UnsupportedOperationException();
    /*try {
      return configuration.getName();
    } catch (Exception e) {
      throw new LaserConsoleException("unable to get configuration name : " + e.getMessage(), e);
    }*/
  }

  public void setName(String newName) throws LaserConsoleException {
  	throw new UnsupportedOperationException();
   /* try {
      try {
        configuration.setName(newName);
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        configuration.setName(newName);
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to set configuration name : " + e.getMessage(), e);
    }*/
  }

  public boolean isDefault() throws LaserConsoleException {
  	throw new UnsupportedOperationException();
    /*try {
      try {
        return configuration.isDefault();
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        return configuration.isDefault();
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to check if the configuration is a default : " + e.getMessage(), e);
    }*/
  }

  public Selection getSelection() throws LaserConsoleException {
  	return new SelectionImpl();
  	//throw new UnsupportedOperationException();
    /*try {
      byte[] data = null;
      try {
        data = configuration.getSelection();
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        data = configuration.getSelection();
      }
      return (Selection) unmarshal(data);
    } catch (Exception e) {
      throw new LaserConsoleException("unable to get configuration selection : " + e.getMessage(), e);
    }*/
  }

  public void setSelection(Selection newSelection) throws LaserConsoleException {
  	throw new UnsupportedOperationException();
  	/*try {
      byte[] data = marshal(newSelection);
      try {
        configuration.setSelection(data);
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        configuration.setSelection(data);
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to set configuration selection : " + e.getMessage(), e);
    }*/
  }

  public Behaviour createBehaviour() {
    return new BehaviourImpl();
  }

  public Behaviour getBehaviour() throws LaserConsoleException {
  	return new BehaviourImpl();
  	//throw new UnsupportedOperationException();
  	/*try {
      byte[] data = null;
      try {
        data = configuration.getBehaviour();
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        data = configuration.getBehaviour();
      }
      return (Behaviour) unmarshal(data);
    } catch (Exception e) {
      throw new LaserConsoleException("unable to get configuration behaviour : " + e.getMessage(), e);
    }*/
  }

  public void setBehaviour(Behaviour newBehaviour) throws LaserConsoleException {
    
  	throw new UnsupportedOperationException();
  	/*try {
      byte[] data = marshal(newBehaviour);
      try {
        configuration.setBehaviour(data);
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        configuration.setBehaviour(data);
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to set configuration behaviour : " + e.getMessage(), e);
    }*/
  }

  public CommentedAlarmMap getMasked() throws LaserConsoleException {
  	throw new UnsupportedOperationException();
  	/*try {
      byte[] data = null;
      try {
        data = configuration.getMasked();
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        data = configuration.getMasked();
      }

      return buildCommentedAlarmMap((Collection) unmarshal(data), true);
    } catch (Exception e) {
      throw new LaserConsoleException("unable to get configuration masked : " + e.getMessage(), e);
    }*/
  }

  public void setMasked(CommentedAlarmMap newMasked) throws LaserConsoleException {
  	throw new UnsupportedOperationException();
  	/*try {
      byte[] data = marshal(buildCommentedAlarmCollection(newMasked));
      try {
        configuration.setMasked(data);
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        configuration.setMasked(data);
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to set configuration masked alarms : " + e.getMessage(), e);
    }*/
  }

  public CommentedAlarmMap getInhibited() throws LaserConsoleException {
  	throw new UnsupportedOperationException();/*
  	try {
      byte[] data = null;
      try {
        data = configuration.getInhibited();
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        data = configuration.getInhibited();
      }
      return buildCommentedAlarmMap((Collection) unmarshal(data), false);
    } catch (Exception e) {
      throw new LaserConsoleException("unable to get configuration inhibited alarms : " + e.getMessage(), e);
    }*/
  }

  public void setInhibited(CommentedAlarmMap newInhibited) throws LaserConsoleException {
  	throw new UnsupportedOperationException();
  	/*
  	try {
      byte[] data = marshal(buildCommentedAlarmCollection(newInhibited));
      try {
        configuration.setInhibited(data);
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        configuration.setInhibited(data);
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to set configuration inhibited alarms : " + e.getMessage(), e);
    }*/
  }

  public CommentedAlarmMap getHighlighted() throws LaserConsoleException {
  	throw new UnsupportedOperationException(); /*
  	try {
      byte[] data = null;
      try {
        data = configuration.getHighlighted();
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        data = configuration.getHighlighted();
      }

      return buildCommentedAlarmMap((Collection) unmarshal(data), true);
    } catch (Exception e) {
      throw new LaserConsoleException("unable to get configuration highlighted alarms : " + e.getMessage(), e);
    }*/
  }

  public void setHighlighted(CommentedAlarmMap newHighlighted) throws LaserConsoleException {
  	throw new UnsupportedOperationException();
  	/*try {
      byte[] data = marshal(buildCommentedAlarmCollection(newHighlighted));
      try {
        configuration.setHighlighted(data);
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        configuration.setHighlighted(data);
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to set configuration highlighted alarms : " + e.getMessage(), e);
    }*/
  }

  public CommentedAlarmMap getAutoHighlighted() throws LaserConsoleException {
  	throw new UnsupportedOperationException();
  	/*try {
      byte[] data = null;
      try {
        data = configuration.getAutoHighlighted();
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        data = configuration.getAutoHighlighted();
      }

      return buildCommentedAlarmMap((Collection) unmarshal(data), false);
    } catch (Exception e) {
      throw new LaserConsoleException("unable to get configuration auto highlighted alarms : " + e.getMessage(), e);
    }*/
  }

  public void setAutoHighlighted(CommentedAlarmMap newAutoHighlighted) throws LaserConsoleException {
  	throw new UnsupportedOperationException();
  	/*try {
      byte[] data = marshal(buildCommentedAlarmCollection(newAutoHighlighted));
      try {
        configuration.setAutoHighlighted(data);
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        configuration.setAutoHighlighted(data);
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to set configuration auto highlighted alarms : " + e.getMessage(), e);
    }*/
  }

  public CommentedAlarmMap getAutoKlaxoned() throws LaserConsoleException {
  	throw new UnsupportedOperationException();
  	/*try {
      byte[] data = null;
      try {
        data = configuration.getAutoKlaxoned();
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        data = configuration.getAutoKlaxoned();
      }

      return buildCommentedAlarmMap((Collection) unmarshal(data), false);
    } catch (Exception e) {
      throw new LaserConsoleException("unable to get configuration auto klaxoned alarms : " + e.getMessage(), e);
    }*/
  }

  public void setAutoKlaxoned(CommentedAlarmMap newAutoKlaxoned) throws LaserConsoleException {
  	throw new UnsupportedOperationException();/*try {
      byte[] data = marshal(buildCommentedAlarmCollection(newAutoKlaxoned));
      try {
        configuration.setAutoKlaxoned(data);
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        configuration.setAutoKlaxoned(data);
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to set configuration auto klaxoned alarms : " + e.getMessage(), e);
    }*/
  }

  public CommentedAlarmMap getAcknowledged() throws LaserConsoleException {
  	throw new UnsupportedOperationException();/* try {
      byte[] data = null;
      try {
        data = configuration.getAcknowledged();
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        data = configuration.getAcknowledged();
      }

      return buildCommentedAlarmMap((Collection) unmarshal(data), true);
    } catch (Exception e) {
      throw new LaserConsoleException("unable to get configuration acknowledged alarms : " + e.getMessage(), e);
    }*/
  }

  public void setAcknowledged(CommentedAlarmMap newAcknowledged) throws LaserConsoleException {
  	throw new UnsupportedOperationException();/* try {
      byte[] data = marshal(buildCommentedAlarmCollection(newAcknowledged));
      try {
        configuration.setAcknowledged(data);
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        configuration.setAcknowledged(data);
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to set configuration acknowledged alarms : " + e.getMessage(), e);
    }*/
  }

  /* (non-Javadoc)
   * @see cern.laser.console.Configuration#getNewIndicator()
   */
  public CommentedAlarmMap getNewIndicator() throws LaserConsoleException {
  	throw new UnsupportedOperationException();/*try {
      byte[] data = null;
      try {
        data = configuration.getNewIndicator();
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        data = configuration.getNewIndicator();
      }

      return buildCommentedAlarmMap((Collection) unmarshal(data), true);
    } catch (Exception e) {
      throw new LaserConsoleException("unable to get configuration new alarms : " + e.getMessage(), e);
    }*/
  }

  /* (non-Javadoc)
   * @see cern.laser.console.Configuration#setNewIndicator(cern.laser.console.CommentedAlarmMap)
   */
  public void setNewIndicator(CommentedAlarmMap newNewIndicator) throws LaserConsoleException {
  	throw new UnsupportedOperationException();/* try {
      byte[] data = marshal(buildCommentedAlarmCollection(newNewIndicator));
      try {
        configuration.setNewIndicator(data);
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        configuration.setNewIndicator(data);
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to set configuration new alarms : " + e.getMessage(), e);
    }*/
  }

  public Boolean getActiveListFont() throws LaserConsoleException {
  	throw new UnsupportedOperationException();/* try {
      try {
        return configuration.getActiveListFont();
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        return configuration.getActiveListFont();
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to get configuration active list font : " + e.getMessage(), e);
    }*/
  }

  /* (non-Javadoc)
   * @see cern.laser.console.Configuration#setNewIndicator(cern.laser.console.CommentedAlarmMap)
   */
  public void setActiveListFont(Boolean isActiveListFont) throws LaserConsoleException {
  	throw new UnsupportedOperationException();/* try {
      try {
        configuration.setActiveListFont(isActiveListFont);
      } catch (Exception e) {
        configuration = getConsoleConfigurationEntityEJB();
        configuration.setActiveListFont(isActiveListFont);
      }
    } catch (Exception e) {
      throw new LaserConsoleException("unable to set configuration active list font : " + e.getMessage(), e);
    }*/
  }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

  private CommentedAlarmMap buildCommentedAlarmMap(Collection commentedAlarmRefs, boolean removeChanged)
      throws Exception {
    CommentedAlarmMap result = new CommentedAlarmMap();
    if ((commentedAlarmRefs == null) || (commentedAlarmRefs.size() == 0)) { return result; }
    Iterator iterator = commentedAlarmRefs.iterator();
    while (iterator.hasNext()) {
      CommentedAlarmRef ref = (CommentedAlarmRef) iterator.next();
      Alarm alarm = AlarmBrowsingHandler.get(contSvcs).getAlarmById(ref.getAlarmId());
      if ((!removeChanged)
          || (removeChanged && alarm.getStatus().getSourceTimestamp().equals(ref.getSourceTimestamp()))) {
        result.put(new CommentedAlarm(alarm, ref.getComment()));
      }
    }

    return result;
  }

  private Collection buildCommentedAlarmCollection(CommentedAlarmMap alarms) {
    Collection result = new ArrayList();
    if ((alarms == null) || (alarms.size() == 0)) { return result; }
    Iterator iterator = alarms.values().iterator();
    while (iterator.hasNext()) {
      CommentedAlarm alarm = (CommentedAlarm) iterator.next();
      result.add(new CommentedAlarmRef(alarm.getAlarm().getAlarmId(),
          alarm.getAlarm().getStatus().getSourceTimestamp(), alarm.getComment()));
    }

    return result;
  }

  private byte[] marshal(Object obj) throws Exception {
    if (obj == null) { return null; }
    ByteArrayOutputStream byte_out_stream = new ByteArrayOutputStream();
    ObjectOutputStream object_out_stream = new ObjectOutputStream(byte_out_stream);
    object_out_stream.writeObject(obj);
    object_out_stream.flush();
    byte[] result = byte_out_stream.toByteArray();
    object_out_stream.close();

    return result;
  }

  private Object unmarshal(byte[] bytes) throws Exception {
    if (bytes == null) { return null; }
    ByteArrayInputStream byte_in_stream = new ByteArrayInputStream(bytes);
    ObjectInputStream object_in_stream = new ObjectInputStream(byte_in_stream);
    Object result = object_in_stream.readObject();
    object_in_stream.close();

    return result;
  }

  private Object getConsoleConfigurationEntityEJB() throws LaserConnectionException,
   LaserConfigurationNotFoundException {
  	throw new UnsupportedOperationException();/* 
    ConsoleConfigurationEntityEJBHome home = null;
    Enumeration contexts = laser.getLaserContexts();
    StringBuffer errors = new StringBuffer();
    while (contexts.hasMoreElements()) {
      try {
        Context context = (Context) contexts.nextElement();
        home = (ConsoleConfigurationEntityEJBHome) PortableRemoteObject.narrow(context
            .lookup("ConsoleConfigurationEntityEJB"), ConsoleConfigurationEntityEJBHome.class);

        return home.findByPrimaryKey(configurationId);
      } catch (ObjectNotFoundException onf) {
        throw new LaserConfigurationNotFoundException("configuration undefined", onf);
      } catch (Exception e) {
        errors.append("\n[");
        errors.append(e.getMessage());
        errors.append("]");
      }
    }
    throw new LaserConnectionException("unable to connect to LASER : " + errors.toString());*/
  }
}
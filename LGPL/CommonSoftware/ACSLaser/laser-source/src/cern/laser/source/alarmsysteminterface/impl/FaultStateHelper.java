/*
 * FaultStateHelper.java
 *
 * Created on February 21, 2003, 5:32 PM
 */
package cern.laser.source.alarmsysteminterface.impl;

import java.sql.Timestamp;
import java.text.ParseException;
import java.util.Date;
import java.util.Enumeration;
import java.util.Properties;

import org.apache.log4j.Category;

import cern.laser.source.alarmsysteminterface.FaultState;
import cern.laser.source.alarmsysteminterface.impl.message.Property;
import alma.acs.util.IsoDateFormat;

/**
 * Helper class for marshaling/unmarshaling to/from FaultState instances and generated FaultState instances for XML
 * transport.
 * 
 * @author fracalde
 */
public class FaultStateHelper {
  /**
   * logging category
   */
  private static Category cat = Category.getInstance(AlarmSystemInterfaceProxy.class.getName());

  /**
   * Default constructor.
   */
  protected FaultStateHelper() {
  }

  /**
   * Marshal a FaultState into a generated FaultState for XML transport.
   * 
   * @param state The FaultState instance
   * @return The generated FaultState
   */
  public static cern.laser.source.alarmsysteminterface.impl.message.FaultState marshal(FaultState state) {
    cern.laser.source.alarmsysteminterface.impl.message.FaultState generated = new cern.laser.source.alarmsysteminterface.impl.message.FaultState();
    generated.setFamily(state.getFamily());
    generated.setMember(state.getMember());
    generated.setCode(state.getCode());
    generated.setDescriptor(state.getDescriptor());
    generated.setUserTimestamp(IsoDateFormat.formatDate(new Date(state.getUserTimestamp().getTime())));

    cern.laser.source.alarmsysteminterface.impl.message.Properties properties = new cern.laser.source.alarmsysteminterface.impl.message.Properties();
    Enumeration names = state.getUserProperties().propertyNames();

    while (names.hasMoreElements()) {
      String name = (String) names.nextElement();
      Property property = new Property();
      property.setName(name);
      property.setValue(state.getUserProperties().getProperty(name));
      properties.addProperty(property);
    }

    generated.setUserProperties(properties);

    return generated;
  }

  /**
   * Unmarshal a generated FaultState into a FaultState instance.
   * 
   * @param generated The generated FaultState
   * @return The FaultState instance
   */
  public static FaultState unmarshal(cern.laser.source.alarmsysteminterface.impl.message.FaultState generated) {
    FaultState state = new FaultStateImpl();
    state.setFamily(generated.getFamily());
    state.setMember(generated.getMember());
    state.setCode(generated.getCode());
    state.setDescriptor(generated.getDescriptor());
    
    Date timestamp;
    try {
    	timestamp=IsoDateFormat.parseIsoTimestamp(generated.getUserTimestamp());
    	state.setUserTimestamp(new Timestamp(timestamp.getTime()));
    } catch (ParseException pe) {
    	System.err.println("Error paring ISO timestamp from "+generated.getUserTimestamp());
    	System.err.println("Setting to surrent time! "+generated.getUserTimestamp());
    	pe.printStackTrace(System.err);
    	state.setUserTimestamp(new Timestamp(System.currentTimeMillis()));
    }

    Properties properties = new Properties();

    if (generated.getUserProperties() != null) {
      Enumeration property_enum = generated.getUserProperties().enumerateProperty();

      while (property_enum.hasMoreElements()) {
        Property property = (Property) property_enum.nextElement();
        properties.setProperty(property.getName(), property.getValue());
      }
    }

    state.setUserProperties(properties);

    return state;
  }
}
package cern.laser.definition.impl;

import java.io.Reader;
import java.io.Writer;
import java.util.Collection;

import alma.acs.container.ContainerServicesBase;
import alma.alarmsystem.AlarmService;

import cern.laser.business.definition.data.AlarmDefinition;
import cern.laser.client.impl.common.AlarmServiceSingleton;
import cern.laser.definition.AlarmDefinitionHandler;
import cern.laser.definition.LaserDefinitionDuplicationException;
import cern.laser.definition.LaserDefinitionException;
import cern.laser.definition.LaserDefinitionNotAllowedException;
import cern.laser.definition.LaserDefinitionNotFoundException;
import cern.laser.definition.LaserDefinitionNotValidException;
import cern.laser.definition.LaserDefinitionXMLException;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.6 $
 */
public class AlarmDefinitionHandlerImpl extends DefinitionHandlerImpl implements AlarmDefinitionHandler {
	private AlarmService alarmService;
	
	/**
 * Creates a new AlarmDefinitionHandlerImpl object.
 *
 * @param userId DOCUMENT ME!
 */
  public AlarmDefinitionHandlerImpl(String userId, ContainerServicesBase contSvcs) throws LaserDefinitionException {
    super(userId);
    try {
		  this.alarmService=AlarmServiceSingleton.getInstance(contSvcs);
	  } catch (Throwable t) {
		  throw new LaserDefinitionException("Error getting the alarm service",t);
	  }
  }

  /**
 * DOCUMENT ME!
 *
 * @param definition DOCUMENT ME!
 *
 * @throws LaserDefinitionException DOCUMENT ME!
 * @throws LaserDefinitionDuplicationException DOCUMENT ME!
 * @throws LaserDefinitionNotValidException DOCUMENT ME!
 * @throws LaserDefinitionNotAllowedException DOCUMENT ME!
 */
  public void createAlarm(AlarmDefinition definition) throws LaserDefinitionException {
	//  alarmService.createAlarm(getUserId(), definition);
	  throw new UnsupportedOperationException();
  }

  /**
 * DOCUMENT ME!
 *
 * @param definitions DOCUMENT ME!
 *
 * @throws LaserDefinitionException DOCUMENT ME!
 * @throws LaserDefinitionDuplicationException DOCUMENT ME!
 * @throws LaserDefinitionNotValidException DOCUMENT ME!
 * @throws LaserDefinitionNotAllowedException DOCUMENT ME!
 */
  public void createAlarms(Collection definitions) throws LaserDefinitionException {
  	throw new UnsupportedOperationException(); /*try {
      getDefinitionServiceSessionEJB().createAlarms(getUserId(), definitions);
    } catch (cern.laser.business.definition.LaserDefinitionDuplicationException de) {
      throw new LaserDefinitionDuplicationException("alarm alredy defined : " + de.getMessage(), de);
    } catch (cern.laser.business.definition.LaserDefinitionNotValidException nve) {
      throw new LaserDefinitionNotValidException("definition not valid : " + nve.getMessage(), nve);
    } catch (cern.laser.business.definition.LaserDefinitionNotAllowedException nae) {
      throw new LaserDefinitionNotAllowedException("not owner", nae);
    } catch (Exception e) {
      throw new LaserDefinitionException("unable to create alarms : " + e.getMessage(), e);
    }*/
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
  	throw new UnsupportedOperationException(); /*Collection result = null;

    try {
      result = getDefinitionServiceSessionEJB().getAlarms(getUserId());
    } catch (cern.laser.business.definition.LaserDefinitionNotAllowedException nae) {
      throw new LaserDefinitionNotAllowedException("not allowed", nae);
    } catch (Exception e) {
      throw new LaserDefinitionException("unable to download alarms : " + e.getMessage(), e);
    }

    cern.laser.definition.impl.generated.AlarmDefinitionList alarm_definition_list = new cern.laser.definition.impl.generated.AlarmDefinitionList();
    Iterator iterator = result.iterator();

    while (iterator.hasNext()) {
      alarm_definition_list.addAlarmDefinition(marshalAlarmDefinition((AlarmDefinition) iterator.next()));
    }

    try {
      Marshaller marshaller = new Marshaller(xmlDefinitionsWriter);
      marshaller.setNoNamespaceSchemaLocation(System.getProperty(XSD_LOCATION_PROPERTY, DEFAULT_XSD_LOCATION));
      marshaller.marshal(alarm_definition_list);
      xmlDefinitionsWriter.flush();
    } catch (Exception e) {
      throw new LaserDefinitionXMLException("unable to marshal the XML definitions : " + e.getMessage(), e);
    }*/
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
  public void removeAlarm(AlarmDefinition definition) throws LaserDefinitionException {
  	throw new UnsupportedOperationException(); /*try {
      getDefinitionServiceSessionEJB().removeAlarm(getUserId(), definition);
    } catch (cern.laser.business.definition.LaserDefinitionNotFoundException nfe) {
      throw new LaserDefinitionNotFoundException("alarm not defined : " + definition.toShortString(), nfe);
    } catch (cern.laser.business.definition.LaserDefinitionNotAllowedException nae) {
      throw new LaserDefinitionNotAllowedException("not owner", nae);
    } catch (Exception e) {
      throw new LaserDefinitionException("unable to remove alarm : " + e.getMessage(), e);
    }*/
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
  public void updateAlarm(AlarmDefinition definition) throws LaserDefinitionException {
  	throw new UnsupportedOperationException(); /* try {
      getDefinitionServiceSessionEJB().updateAlarm(getUserId(), definition);
    } catch (cern.laser.business.definition.LaserDefinitionNotValidException nve) {
      throw new LaserDefinitionNotValidException("definition not valid : " + definition, nve);
    } catch (cern.laser.business.definition.LaserDefinitionNotFoundException nfe) {
      throw new LaserDefinitionNotFoundException("alarm not defined : " + definition, nfe);
    } catch (cern.laser.business.definition.LaserDefinitionNotAllowedException nae) {
      throw new LaserDefinitionNotAllowedException("not owner", nae);
    } catch (Exception e) {
      throw new LaserDefinitionException("unable to get alarm : " + e.getMessage(), e);
    }*/
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
  	throw new UnsupportedOperationException(); /* try {
      getDefinitionServiceSessionEJB().uploadAlarms(getUserId(), toBeCreated, toBeUpdated, toBeRemoved);
    } catch (cern.laser.business.definition.LaserDefinitionDuplicationException de) {
      throw new LaserDefinitionDuplicationException("alarm alredy defined : " + de.getMessage(), de);
    } catch (cern.laser.business.definition.LaserDefinitionNotFoundException nfe) {
      throw new LaserDefinitionNotFoundException("alarm not defined : " + nfe.getMessage(), nfe);
    } catch (cern.laser.business.definition.LaserDefinitionNotValidException nve) {
      throw new LaserDefinitionNotValidException("definition not valid : " + nve.getMessage(), nve);
    } catch (cern.laser.business.definition.LaserDefinitionNotAllowedException nae) {
      throw new LaserDefinitionNotAllowedException("not owner", nae);
    } catch (Exception e) {
      throw new LaserDefinitionException("unable to upload alarms : " + e.getMessage(), e);
    }*/
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
  	throw new UnsupportedOperationException(); /*cern.laser.definition.impl.generated.AlarmDefinitions alarm_definitions = null;

    try {
      alarm_definitions = (cern.laser.definition.impl.generated.AlarmDefinitions) Unmarshaller.unmarshal(cern.laser.definition.impl.generated.AlarmDefinitions.class, xmlDefinitionsReader);
    } catch (Exception e) {
      throw new LaserDefinitionXMLException("unable to unmarshal the XML defintions : " + e.getMessage(), e);
    }

    Enumeration alarms_enumeration = null;

    // to be created
    Set to_be_created = new HashSet();

    if (alarm_definitions.getAlarmsToCreate() != null) {
      alarms_enumeration = alarm_definitions.getAlarmsToCreate().enumerateAlarmDefinition();

      while (alarms_enumeration.hasMoreElements()) {
        to_be_created.add(unmarshalAlarmDefinition((cern.laser.definition.impl.generated.AlarmDefinition) alarms_enumeration.nextElement()));
      }
    }

    // to be updated
    Set to_be_updated = new HashSet();

    if (alarm_definitions.getAlarmsToUpdate() != null) {
      alarms_enumeration = alarm_definitions.getAlarmsToUpdate().enumerateAlarmDefinition();

      while (alarms_enumeration.hasMoreElements()) {
        to_be_updated.add(unmarshalAlarmDefinition((cern.laser.definition.impl.generated.AlarmDefinition) alarms_enumeration.nextElement()));
      }
    }

    // to be removed
    Set to_be_removed = new HashSet();

    if (alarm_definitions.getAlarmsToRemove() != null) {
      alarms_enumeration = alarm_definitions.getAlarmsToRemove().enumerateAlarmDefinition();

      while (alarms_enumeration.hasMoreElements()) {
        to_be_removed.add(unmarshalAlarmDefinition((cern.laser.definition.impl.generated.AlarmDefinition) alarms_enumeration.nextElement()));
      }
    }

    upload(to_be_created, to_be_updated, to_be_removed);*/
  }

  private Object marshalAlarmDefinition(AlarmDefinition definition) {
  	throw new UnsupportedOperationException(); /*cern.laser.definition.impl.generated.AlarmDefinition result = new cern.laser.definition.impl.generated.AlarmDefinition();
    result.setFaultFamily(definition.getFaultFamily());
    result.setFaultMember(definition.getFaultMember());
    result.setFaultCode(definition.getFaultCode().intValue());

    cern.laser.definition.impl.generated.VisualFields visual_fields = new cern.laser.definition.impl.generated.VisualFields();
    visual_fields.setSystemName(definition.getSystemName());
    visual_fields.setIdentifier(definition.getIdentifier());
    visual_fields.setProblemDescription(definition.getProblemDescription());
    result.setVisualFields(visual_fields);
    result.setCause(definition.getCause());
    result.setAction(definition.getAction());
    result.setConsequence(definition.getConsequence());
    result.setHelpUrl(definition.getHelpURL());
    result.setInstant(definition.getInstant().booleanValue());
    result.setPiquetGSM(definition.getPiquetGSM());
    result.setPriority((definition.getPriority() == null) ? 1 : definition.getPriority().intValue());
    result.setResponsibleId((definition.getResponsiblePersonId() == null) ? 1 : definition.getResponsiblePersonId().intValue());
    result.setSourceName(definition.getSourceName());

    cern.laser.definition.impl.generated.Location location = new cern.laser.definition.impl.generated.Location();
    location.setBuilding(definition.getBuilding());
    location.setFloor(definition.getFloor());
    location.setMnemonic(definition.getMnemonic());
    location.setPosition(definition.getPosition());
    location.setRoom(definition.getRoom());
    result.setLocation(location);

    return result;*/
  }

  private AlarmDefinition unmarshalAlarmDefinition(Object definition) {
  	throw new UnsupportedOperationException(); /*AlarmDefinition result = new AlarmDefinition(definition.getFaultFamily(), definition.getFaultMember(), new Integer(definition.getFaultCode()));
    if (definition.getVisualFields() != null) {
      result.setSystemName(definition.getVisualFields().getSystemName());
      result.setIdentifier(definition.getVisualFields().getIdentifier());
      result.setProblemDescription(definition.getVisualFields().getProblemDescription());
    }
    result.setPriority(new Integer(definition.getPriority()));
    result.setCause(definition.getCause());
    result.setAction(definition.getAction());
    result.setConsequence(definition.getConsequence());
    result.setInstant(new Boolean(definition.getInstant()));
    result.setHelpURL(definition.getHelpUrl());
    result.setSourceName(definition.getSourceName());
    if (definition.getLocation() != null) {
      result.setBuilding(definition.getLocation().getBuilding());
      result.setFloor(definition.getLocation().getFloor());
      result.setRoom(definition.getLocation().getRoom());
      result.setMnemonic(definition.getLocation().getMnemonic());
      result.setPosition(definition.getLocation().getPosition());
    }
    result.setResponsiblePersonId(new Integer(definition.getResponsibleId()));
    result.setPiquetGSM(definition.getPiquetGSM());
    result.setPiquetEmail(definition.getPiquetEmail());

    return result;*/
  }
}

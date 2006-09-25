/*
 * $Id: AlarmImpl.java,v 1.8 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.8 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.data;

import java.io.Serializable;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import cern.laser.business.definition.data.AlarmDefinition;

/**
 * 
 * 
 * @version $Revision: 1.8 $ $Date: 2006/09/25 08:52:36 $
 * @author Niall Stapley
 * @author Katarina Sigerud
 * 
 * @hibernate.class table="ALARM_DEFINITION"
 *  
 */
public class AlarmImpl implements Serializable, Cloneable, Alarm {
  private String alarmId;
  private String systemName;
  private String identifier;
  private String problemDescription;
  private Integer priority;
  private String cause;
  private String action;
  private String consequence;
  private String piquetGSM;
  private String piquetEmail;
  private String helpURLString;
  private Boolean instant;
  private Source source;
  private Location location;
  private ResponsiblePerson responsiblePerson;
  private Set categories;
  private Set nodeParentIds;
  private Set nodeChildrenIds;
  private Set multiplicityParentIds;
  private Set multiplicityChildrenIds;
  private Integer multiplicityThreshold;
  private Status status = Status.INITIAL_STATUS;
  private Triplet triplet = new Triplet();

	/**
	 * @param alarmId
	 * @param systemName
	 * @param identifier
	 * @param problemDescription
	 * @param priority
	 * @param cause
	 * @param action
	 * @param consequence
	 * @param piquetGSM
	 * @param piquetEmail
	 * @param helpURLString
	 * @param instant
	 * @param source
	 * @param location
	 * @param responsiblePerson
	 * @param categories
	 * @param multiplicityThreshold
	 * @param status
	 * @param triplet
	 */
	public AlarmImpl(String alarmId, String systemName, String identifier,
			String problemDescription, Integer priority, String cause,
			String action, String consequence, String piquetGSM,
			String piquetEmail, String helpURLString, Boolean instant,
			Source source, Location location,
			ResponsiblePerson responsiblePerson, Set categories,
			Status status, Triplet triplet,

			boolean nodeParent,	boolean multiplicityParent,
			boolean nodeChild, boolean multiplicityChild) {
		
		this.alarmId = alarmId;
		this.systemName = systemName;
		this.identifier = identifier;
		this.problemDescription = problemDescription;
		this.priority = priority;
		this.cause = cause;
		this.action = action;
		this.consequence = consequence;
		this.piquetGSM = piquetGSM;
		this.piquetEmail = piquetEmail;
		this.helpURLString = helpURLString;
		this.instant = instant;
		this.source = source;
		this.location = location;
		this.responsiblePerson = responsiblePerson;
		this.categories = categories;
		this.status = status;
		this.triplet = triplet;
		
		// just fill w/ something, that hasNodeParents() will return true
		if (nodeParent)
		{
			nodeParentIds = new HashSet(1);
			nodeParentIds.add(new Integer(-1));
		}
			
		// just fill w/ something, that hasNodeChildren() will return true
		if (nodeChild)
		{
			nodeChildrenIds = new HashSet(1);
			nodeChildrenIds.add(new Integer(-1));
		}

		// just fill w/ something, that hasMultiplicityParents() will return true
		if (multiplicityParent)
		{
			multiplicityParentIds = new HashSet(1);
			multiplicityParentIds.add(new Integer(-1));
		}

		// just fill w/ something, that hasMultiplicityChildren() will return true
		if (multiplicityChild)
		{
			multiplicityChildrenIds = new HashSet(1);
			multiplicityChildrenIds.add(new Integer(-1));
		}

	}
  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  /**
   * Empty constructor for Hibernate.
   *  
   */
  public AlarmImpl() {
    //TODO make the default constructor private
  }

  /**
   * @param responsiblePerson
   * @param source
   * @param alarm
   */
  public AlarmImpl(AlarmDefinition definition, Source source, ResponsiblePerson responsiblePerson) {
    setAlarmId(Triplet
        .toIdentifier(definition.getFaultFamily(), definition.getFaultMember(), definition.getFaultCode()));
    setTriplet(new Triplet(definition.getFaultFamily(), definition.getFaultMember(), definition.getFaultCode()));
    setDefinition(definition);
    setSource(source);
    setResponsiblePerson(responsiblePerson);
    setMultiplicityThreshold(new Integer(0));
    setSystemName(definition.getSystemName());
    status = (Status) ((StatusImpl) Status.INITIAL_STATUS).clone();
    status.setStatusId(getAlarmId());
 }
	
  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  /**
   * @hibernate.id generator-class="assigned" column="ALARM_ID"
   */
  public String getAlarmId() {
    return getTriplet().toIdentifier();
  }

  /**
   * @return Returns the action.
   * @hibernate.property name="getAction" column="ACTION" not-null="false" unique="false"
   */
  public String getAction() {
    return action;
  }

  /**
   * @return Returns the categories.
   * @hibernate.set name="getCategories" table="ALARM_CATEGORY" inverse="true" lazy="true"
   * @hibernate.collection-key column="ALARM_ID"
   * @hibernate.collection-many-to-many column="CATEGORY_ID" class="cern.laser.business.data.CategoryImpl"
   */
  public Collection getCategories() {
    return categories == null ? new HashSet(0) : categories;
  }

  /**
   * @param categories The categories to set.
   */
  public void setCategories(Collection newCategories) {
    categories = (Set) newCategories;
  }

  /**
   * @return Returns the cause.
   * @hibernate.property name="getCause" column="CAUSE" not-null="false" unique="false"
   */
  public String getCause() {
    return cause;
  }

  /**
   * @return Returns the consequence.
   * @hibernate.property name="getConsequence" column="CONSEQUENCE" not-null="false" unique="false"
   */
  public String getConsequence() {
    return consequence;
  }

  /**
   * @return Returns the identifier.
   * @hibernate.property name="getIdentifier" column="IDENTIFIER" not-null="false" unique="false"
   */
  public String getIdentifier() {
  	return getTriplet().toIdentifier();
  }

  /**
   * @return Returns the instant.
   * @hibernate.property name="getInstant" column="INSTANT" type="yes_no" not-null="false" unique="false"
   */
  public Boolean getInstant() {
    return instant;
  }

  public void setHelpURL(URL newHelpURL) {
    setHelpURLString(newHelpURL == null ? null : newHelpURL.toExternalForm());
  }

  /**
   * Returns the help URL for this alarm, or <code>null</code> if there is none or if it is malformed.
   * 
   * @return the help URL for this alarm, or <code>null</code> if there is none or if it is malformed.
   */
  public URL getHelpURL() {
    try {
      return new URL(getHelpURLString());
    } catch (MalformedURLException e) {
      return null;
    }
  }

  /**
   * @return Returns the location. 
   * This was mapped as one-to-one, but in the future the table Location will have its own id.
   * @hibernate.many-to-one column="LOCATION_ID" class="cern.laser.business.data.Location" cascade="all" unique="true"
   */
  public Location getLocation() {
    return location;
  }

  /**
   * Returns the ids of the multiplicity children alarms.
   * 
   * @return the ids of the multiplicity children alarms.
   */
  public String[] getMultiplicityChildren() {
    return (String[]) getMultiplicityChildrenIds().toArray(new String[getMultiplicityChildrenIds().size()]);
  }

  /**
   * @return
   */
  public boolean hasMultiplicityChildren() {
    return !getMultiplicityChildrenIds().isEmpty();
  }

  /**
   * @return
   */
  public boolean hasMultiplicityParents() {
    return !getMultiplicityParentIds().isEmpty();
  }

  /**
   * Returns the ids of the multiplicity parent alarms.
   * 
   * @return the ids of the multiplicity parent alarms.
   */
  public String[] getMultiplicityParents() {
    return (String[]) getMultiplicityParentIds().toArray(new String[getMultiplicityParentIds().size()]);
  }

  /**
   * Sets up the bi-directional relation between the multiplicity parent and the multiplicity child
   * 
   * @param multiplicityChild the alarm to add to this alarm as a multiplicity child. This alarm will at the same time
   *          be added to the child's multiplicity parents.
   */
  public void addMultiplicityChild(Alarm multiplicityChild) {
    ((AlarmImpl) multiplicityChild).getMultiplicityParentIds().add(getAlarmId());
    getMultiplicityChildrenIds().add(multiplicityChild.getAlarmId());
  }

  /**
   * Removes the bi-directional relation between the multiplicity parent and the multiplicity child
   * 
   * @param multiplicityChild the alarm to remove from this alarm as a multiplicity child. This alarm will at the same
   *          time be removed from the child's multiplicity parents.
   */
  public void removeMultiplicityChild(Alarm multiplicityChild) {
    ((AlarmImpl) multiplicityChild).getMultiplicityParentIds().remove(this);
    getMultiplicityChildrenIds().remove(multiplicityChild);
  }

  /**
   * Returns the ids of the node children alarms.
   * 
   * @return the ids of the node children alarms.
   */
  public String[] getNodeChildren() {
    return (String[]) getNodeChildrenIds().toArray(new String[getNodeChildrenIds().size()]);
  }

  /**
   * @return
   */
  public boolean hasNodeChildren() {
    return !getNodeChildrenIds().isEmpty();
  }

  /**
   * Returns the ids of the node parent alarms.
   * 
   * @return the ids of the node parent alarms.
   */
  public String[] getNodeParents() {
    return (String[]) getNodeParentIds().toArray(new String[getNodeParentIds().size()]);
  }

  /**
   * @return
   */
  public boolean hasNodeParents() {
    return !getNodeParentIds().isEmpty();
  }

  /**
   * Sets up the bi-directional relation between the node parent and the node child
   * 
   * @param nodeChild the alarm to add to this alarm as a node child. This alarm will at the same time be added to the
   *          child's node parents.
   */
  public void addNodeChild(Alarm nodeChild) {
    ((AlarmImpl) nodeChild).getNodeParentIds().add(getAlarmId());
    getNodeChildrenIds().add(nodeChild.getAlarmId());
  }

  /**
   * Removes the bi-directional relation between the node parent and the multiplicity child
   * 
   * @param nodeChild the alarm to remove from this alarm as a node child. This alarm will at the same time be removed
   *          from the child's node parents.
   */
  public void removeNodeChild(Alarm nodeChild) {
    ((AlarmImpl) nodeChild).getNodeParentIds().remove(this);
    getNodeChildrenIds().remove(nodeChild);
  }

  /**
   * @hibernate.property name="getMultiplicityThreshold" column="MULTIPLICITY_THRESHOLD" not-null="false"
   *                     unique="false"
   */
  public Integer getMultiplicityThreshold() {
    return multiplicityThreshold;
  }

  /**
   */
  public void setMultiplicityThreshold(Integer multiplicityThreshold) {
    this.multiplicityThreshold = multiplicityThreshold;
  }

  /**
   * @hibernate.property name="getPiquetEmail" column="PIQUET_EMAIL" not-null="false" unique="false"
   */
  public String getPiquetEmail() {
    return piquetEmail;
  }

  /**
   * @hibernate.property name="getPiquetGSM" column="PIQUET_PORTABLE_PHONE" not-null="false"
   *                     unique="false"
   */
  public String getPiquetGSM() {
    return piquetGSM;
  }

  /**
   * @hibernate.property name="getPriority" column="PRIORITY" not-null="false" unique="false"
   */
  public Integer getPriority() {
    return priority;
  }

  /**
   * @hibernate.property name="getProblemDescription" column="PROBLEM_DESCRIPTION" not-null="false"
   *                     unique="false"
   */
  public String getProblemDescription() {
    return problemDescription;
  }

  /**
   * @hibernate.many-to-one name="getResponsiblePerson" column="RESPONSIBLE_ID"
   *                        class="cern.laser.business.data.ResponsiblePerson" not-null="false"
   */
  public ResponsiblePerson getResponsiblePerson() {
    return responsiblePerson;
  }

  /**
   * @hibernate.many-to-one name="getSource" column="SOURCE_ID" class="cern.laser.business.data.Source"
   * 
   *  
   */
  public Source getSource() {
    return source;
  }

  /**
   * @return Returns the status.
   * @hibernate.many-to-one column="STATUS_ID" class="cern.laser.business.data.StatusImpl" 
   *                                            cascade="all" unique="true"
   */
  public Status getStatus() {
    return status;
  }

  /**
   * @param status The status to set.
   */
  public void setStatus(Status status) {
    this.status = status;
  }

  /**
   * @return Returns the systemName.
   * @hibernate.property name="getSystemName" column="SYSTEM_NAME" not-null="false" unique="false"
   */
  public String getSystemName() {
    return systemName;
  }

  /**
   * @return Returns the triplet.
   */
  public Triplet getTriplet() {
    return triplet;
  }

  /**
   * @return
   */
  public AlarmDefinition getDefinition() {
    AlarmDefinition definition = new AlarmDefinition(getFaultFamily(), getFaultMember(), getFaultCode());
    definition.setSystemName(getSystemName());
    definition.setIdentifier(getIdentifier());
    definition.setProblemDescription(getProblemDescription());
    definition.setCause(getCause());
    definition.setAction(getAction());
    definition.setConsequence(getConsequence());
    definition.setInstant(getInstant());
    definition.setPriority(getPriority());
    definition.setMnemonic(location.getMnemonic());
    definition.setFloor(location.getFloor());
    definition.setRoom(location.getRoom());
    definition.setPosition(location.getPosition());
    definition.setPiquetGSM(getPiquetGSM());
    definition.setHelpURL(getHelpURLString());
    definition
        .setResponsiblePersonId(getResponsiblePerson() == null ? null : getResponsiblePerson().getResponsibleId());
    definition.setSourceName(getSource() == null ? null : getSource().getName());

    definition.setBuilding(location.getBuilding() == null ? null : location.getBuilding().getBuildingNumber());

    return definition;
  }

  public void setDefinition(AlarmDefinition definition) {
    setSystemName(definition.getSystemName());
    setIdentifier(definition.getIdentifier());
    setProblemDescription(definition.getProblemDescription());
    setCause(definition.getCause());
    setAction(definition.getAction());
    setConsequence(definition.getConsequence());
    setInstant(definition.getInstant());
    setPriority(definition.getPriority());

    setLocation(new Location(definition.getAlarmId(), definition.getFloor(), definition
        .getMnemonic(), definition.getPosition(), definition.getRoom()));

    setPiquetGSM(definition.getPiquetGSM());
    setPiquetEmail(definition.getPiquetEmail());

    try {
      setHelpURL(new URL(definition.getHelpURL()));
    } catch (MalformedURLException e) {
      setHelpURL(null);
    }
  }

  /**
   * @param action The action to set.
   */
  public void setAction(String action) {
    this.action = action;
  }

  /**
   * @param alarmId The alarmId to set.
   */
  public void setAlarmId(String alarmId) {
    this.alarmId = alarmId;
  }

  /**
   * @param cause The cause to set.
   */
  public void setCause(String cause) {
    this.cause = cause;
  }

  /**
   * @param consequence The consequence to set.
   */
  public void setConsequence(String consequence) {
    this.consequence = consequence;
  }

  /**
   * @param identifier The identifier to set.
   */
  public void setIdentifier(String identifier) {
    this.identifier = identifier;
  }

  /**
   * @param instant The instant to set.
   */
  public void setInstant(Boolean instant) {
    this.instant = instant;
  }

  /**
   * @param location The location to set.
   */
  public void setLocation(Location location) {
    this.location = location;
  }

  /**
   * @param piquetEmail The piquetEmail to set.
   */
  public void setPiquetEmail(String piquetEmail) {
    this.piquetEmail = piquetEmail;
  }

  /**
   * @param piquetGSM The piquetGSM to set.
   */
  public void setPiquetGSM(String piquetGSM) {
    this.piquetGSM = piquetGSM;
  }

  /**
   * @param priority The priority to set.
   */
  public void setPriority(Integer priority) {
    this.priority = priority;
  }

  /**
   * @param problemDescription The problemDescription to set.
   */
  public void setProblemDescription(String problemDescription) {
    this.problemDescription = problemDescription;
  }

  /**
   * @param responsiblePerson The responsiblePerson to set.
   */
  public void setResponsiblePerson(ResponsiblePerson responsiblePerson) {
    this.responsiblePerson = responsiblePerson;
  }

  /**
   * @param source The source to set.
   */
  public void setSource(Source source) {
    this.source = source;
  }

  /**
   * @param systemName The systemName to set.
   */
  public void setSystemName(String systemName) {
    this.systemName = systemName;
  }

  /**
   * @param triplet The triplet to set.
   */
  public void setTriplet(Triplet triplet) {
    this.triplet = triplet;
  }

  public Object clone() {
    try {
      AlarmImpl alarm = (AlarmImpl) super.clone();
      alarm.setLocation(location == null ? null : (Location) location.clone());
      alarm.setResponsiblePerson(responsiblePerson == null ? null : (ResponsiblePerson) responsiblePerson.clone());
      alarm.setSource(source == null ? null : (Source) source.clone());
      alarm.setStatus((Status) (status == null ? null : ((StatusImpl) status).clone()));
      alarm.setTriplet(triplet == null ? null : (Triplet) triplet.clone());
      alarm.setSystemName(systemName);
      if (categories == null) {
        alarm.setCategories(null);
      } else {
        Set categories_copy = new HashSet();
        Iterator iterator = categories.iterator();
        while (iterator.hasNext()) {
          categories_copy.add(((CategoryImpl) iterator.next()).clone());
        }
        alarm.setCategories(categories_copy);
      }

      alarm.setMultiplicityChildrenIds(new HashSet(
          getMultiplicityChildrenIds()));
      alarm.setMultiplicityParentIds(new HashSet(getMultiplicityParentIds()));
      alarm.setNodeChildrenIds(new HashSet(getNodeChildrenIds()));
      alarm.setNodeParentIds(new HashSet(getNodeParentIds()));

      return alarm;
    } catch (Exception e) {
      throw new InternalError("unable to clone alarm " + getTriplet() + " : " + e.getMessage());
    }
  }

  public String toString() {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nALARM :");
    str_buf.append("\nID : ");
    str_buf.append(alarmId);
    str_buf.append("\nTRIPLET : ");
    str_buf.append(triplet);
    str_buf.append("\nSYSTEM NAME : ");
    str_buf.append(systemName);
    str_buf.append("\nIDENTIFIER : ");
    str_buf.append(identifier);
    str_buf.append("\nPROBLEM DESCRIPTION : ");
    str_buf.append(problemDescription);
    str_buf.append("\nPRIORITY : ");
    str_buf.append(priority);
    str_buf.append("\nCAUSE : ");
    str_buf.append(cause);
    str_buf.append("\nACTION : ");
    str_buf.append(action);
    str_buf.append("\nCONSEQUENCE : ");
    str_buf.append(consequence);
    str_buf.append("\nHELP URL : ");
    str_buf.append(helpURLString);
    str_buf.append("\nINSTANT : ");
    str_buf.append(instant);
    str_buf.append("\nLOCATION : ");
    str_buf.append(location);
    str_buf.append("\nSOURCE : ");
    str_buf.append(source);
    str_buf.append("\nPIQUET GSM : ");
    str_buf.append(piquetGSM);
    str_buf.append("\nRESPONSIBLE : ");
    str_buf.append(responsiblePerson);
    str_buf.append("\nSTATUS : ");
    str_buf.append(status);
    str_buf.append("\nCATEGORIES : ");
    str_buf.append(getCategories());
    str_buf.append("\nNODE PARENTS : ");
    str_buf.append(getNodeParentIds());
    str_buf.append("\nNODE CHILDREN : ");
    str_buf.append(getNodeChildrenIds());
    str_buf.append("\nMULTIPLICTY PARENTS : ");
    str_buf.append(getMultiplicityParentIds());
    str_buf.append("\nMULTIPLICTY CHILDREN : ");
    str_buf.append(getMultiplicityChildrenIds());
    str_buf.append("\nMULTIPLICTY THRESHOLD : ");
    str_buf.append(multiplicityThreshold);

    return str_buf.toString();
  }

  public int hashCode() {
    return getAlarmId().hashCode();
  }

  public boolean equals(Object obj) {
    if (obj == null) { return false; }
    if (!(obj instanceof Alarm)) { return false; }
    Alarm alarm = (Alarm) obj;

    return getAlarmId().equals(alarm.getAlarmId());
  }

  //
  // -- PRIVATE METHODS ----------------------------------------------
  //

  /**
   * @return Returns the faultFamily.
   * @hibernate.property name="getFaultFamily" column="FAULT_FAMILY" not-null="false" unique="true"
   */
  private String getFaultFamily() {
    return triplet.getFaultFamily();
  }

  /**
   * @param faultFamily The faultFamily to set.
   */
  private void setFaultFamily(String faultFamily) {
    triplet.setFaultFamily(faultFamily);
  }

  /**
   * @return Returns the faultMember.
   * @hibernate.property name="getFaultMember" column="FAULT_MEMBER" not-null="false" unique="false"
   */
  private String getFaultMember() {
    return triplet.getFaultMember();
  }

  /**
   * @param faultMember The faultMember to set.
   */
  private void setFaultMember(String faultMember) {
    triplet.setFaultMember(faultMember);
  }

  /**
   * @return Returns the faultCode.
   * @hibernate.property name="getFaultCode" column="FAULT_CODE" not-null="false" unique="false"
   */
  private Integer getFaultCode() {
    return triplet.getFaultCode();
  }

  /**
   * @param faultCode The faultCode to set.
   */
  private void setFaultCode(Integer faultCode) {
    triplet.setFaultCode(faultCode);
  }

  /**
   * @hibernate.property name="getHelpURLString" column="HELP_URL" not-null="false" unique="false"
   * 
   * @return url
   */
  ///**
  //* @hibernate.property name="getHelpURL" column="HELP_URL"
  //* type="cern.laser.business.data.customtypes.JavaNetURLUserType" not-null="false" unique="false"
  //*/
  public String getHelpURLString() {
    return helpURLString;
  }

  /**
   * @param newHelpURL The helpURLString to set.
   */
  public void setHelpURLString(String newHelpURL) {
    this.helpURLString = newHelpURL;
  }

  /**
   * Returns the ids of the associated multiplicity children
   * @return the ids of the associated multiplicity children
   * 
   * @hibernate.set name="getMultiplicityChildrenIds" table="MULTIPLICITY_REDUCTION" inverse="true" lazy="true"
   * @hibernate.collection-key column="PARENT_ID"
   * @hibernate.collection-element column="CHILD_ID" type="string"
   */
  public Set getMultiplicityChildrenIds() {
    return multiplicityChildrenIds == null ? new HashSet(0) : multiplicityChildrenIds;
  }

  /**
   * @param newMultiplicityChildrenIds The new collection of multiplicity children to set.
   */
  public void setMultiplicityChildrenIds(Set newMultiplicityChildrenIds) {
    multiplicityChildrenIds = newMultiplicityChildrenIds;
  }

  /**
   * Returns the ids of the associated multiplicity parents
   * @return the ids of the associated multiplicity parents
   * 
   * @hibernate.set name="getMultiplicityParentIds" table="MULTIPLICITY_REDUCTION" inverse="false" lazy="true"
   * @hibernate.collection-key column="CHILD_ID"
   * @hibernate.collection-element column="PARENT_ID" type="string"
   */
  public Set getMultiplicityParentIds() {
    return multiplicityParentIds == null ? new HashSet(0) : multiplicityParentIds;
  }

  /**
   * @param newMultiplicityParentIds The new collection of multiplicity parents to set.
   */
  public void setMultiplicityParentIds(Set newMultiplicityParentIds) {
    multiplicityParentIds = newMultiplicityParentIds;
  }

  /**
   * Returns the ids of the associated node children
   * @return the ids of the associated node children
   * 
   * @hibernate.set name="getNodeChildrenIds" table="NODE_REDUCTION" inverse="true" lazy="true"
   * @hibernate.collection-key column="PARENT_ID"
   * @hibernate.collection-element column="CHILD_ID" type="string"
   */
  public Set getNodeChildrenIds() {
    return nodeChildrenIds == null ? new HashSet(0) : nodeChildrenIds;
  }

  /**
   * @param newNnodeChildrenIds The nodeChildrenId to set.
   */
  public void setNodeChildrenIds(Set newNnodeChildrenIds) {
    nodeChildrenIds = newNnodeChildrenIds;
  }

  /**
   * Returns the ids of the associated node parents
   * @return the ids of the associated node parents
   * 
   * @hibernate.set name="getNodeParentIds" table="NODE_REDUCTION" inverse="false" lazy="true"
   * @hibernate.collection-key column="CHILD_ID"
   * @hibernate.collection-element column="PARENT_ID" type="string"
   */
  public Set getNodeParentIds() {
    return nodeParentIds == null ? new HashSet(0) : nodeParentIds;
  }

  /**
   * @param nodeParentIds The new collection of node parents to set.
   */
  public void setNodeParentIds(Set newNodeParentIds) {
    nodeParentIds = newNodeParentIds;
  }
}
package cern.laser.business.definition.data;

import java.io.Serializable;

import cern.laser.business.data.Triplet;

public class AlarmDefinition implements Serializable, Cloneable {
  private String faultFamily;
  private String faultMember;
  private Integer faultCode;
  private String systemName;
  private String identifier;
  private String problemDescription;
  private Integer priority;
  private String cause;
  private String action;
  private String consequence;
  private Boolean instant;
  private String helpURL;
  private String building;
  private String mnemonic;
  private String floor;
  private String room;
  private String position;
  private String sourceName;
  private Integer responsiblePersonId;
  private String piquetGSM;
  private String piquetEmail;

  public AlarmDefinition(String faultFamily, String faultMember, Integer faultCode) {
    setFaultFamily(faultFamily);
    setFaultMember(faultMember);
    setFaultCode(faultCode);
  }

  public AlarmDefinition(String faultFamily, String faultMember, Integer faultCode, String systemName,
      String identifier, String problemDescription, Integer priority, String cause, String action, String consequence,
      Boolean instant, String helpURL, String sourceName, String building, String floor, String room, String mnemonic,
      String position, Integer responsiblePersonId, String piquetGSM, String piquetEmail) {
    setFaultFamily(faultFamily);
    setFaultMember(faultMember);
    setFaultCode(faultCode);
    setSystemName(systemName);
    setIdentifier(identifier);
    setProblemDescription(problemDescription);
    setPriority(priority);
    setCause(cause);
    setAction(action);
    setConsequence(consequence);
    setInstant(instant);
    setHelpURL(helpURL);
    setSourceName(sourceName);
    setBuilding(building);
    setFloor(floor);
    setRoom(room);
    setMnemonic(mnemonic);
    setPosition(position);
    setResponsiblePersonId(responsiblePersonId);
    setPiquetGSM(piquetGSM);
    setPiquetEmail(piquetEmail);
  }

  public String getAlarmId() {
    return Triplet.toIdentifier(faultFamily, faultMember, faultCode);
  }

  public Object clone() {
    try {
      AlarmDefinition alarm = (AlarmDefinition) super.clone();

      return alarm;
    } catch (Exception e) {
      throw new InternalError();
    }
  }

  public String getFaultFamily() {
    return faultFamily;
  }

  public String getFaultMember() {
    return faultMember;
  }

  public Integer getFaultCode() {
    return faultCode;
  }

  public void setFaultFamily(String newFaultFamily) {
    faultFamily = newFaultFamily;
  }

  public void setFaultMember(String newFaultMember) {
    faultMember = newFaultMember;
  }

  public void setFaultCode(Integer newFaultCode) {
    faultCode = newFaultCode;
  }

  public String getProblemDescription() {
    return problemDescription;
  }

  public void setProblemDescription(String newProblemDescription) {
    problemDescription = newProblemDescription;
  }

  public Integer getPriority() {
    return priority;
  }

  public void setPriority(Integer newPriority) {
    priority = newPriority;
  }

  public String getSourceName() {
    return sourceName;
  }

  public void setSourceName(String newSourceName) {
    sourceName = newSourceName;
  }

  public String getHelpURL() {
    return helpURL;
  }

  public void setHelpURL(String newHelpURL) {
    helpURL = newHelpURL;
  }

  public Integer getResponsiblePersonId() {
    return responsiblePersonId;
  }

  public void setResponsiblePersonId(Integer newResponsiblePersonId) {
    responsiblePersonId = newResponsiblePersonId;
  }

  public Boolean getInstant() {
    return instant;
  }

  public void setInstant(Boolean newInstant) {
    instant = newInstant;
  }

  public String getSystemName() {
    return systemName;
  }

  public void setSystemName(String newSystemName) {
    systemName = newSystemName;
  }

  public String getIdentifier() {
    return identifier;
  }

  public void setIdentifier(String newIdentifier) {
    identifier = newIdentifier;
  }

  public String getCause() {
    return cause;
  }

  public void setCause(String newCause) {
    cause = newCause;
  }

  public String getConsequence() {
    return consequence;
  }

  public void setConsequence(String newConsequence) {
    consequence = newConsequence;
  }

  public String getAction() {
    return action;
  }

  public void setAction(String newAction) {
    action = newAction;
  }

  public String getBuilding() {
    return building;
  }

  public void setBuilding(String newBuilding) {
    building = newBuilding;
  }

  public String getMnemonic() {
    return mnemonic;
  }

  public void setMnemonic(String newMnemonic) {
    mnemonic = newMnemonic;
  }

  public String getFloor() {
    return floor;
  }

  public void setFloor(String newFloor) {
    floor = newFloor;
  }

  public String getRoom() {
    return room;
  }

  public void setRoom(String newRoom) {
    room = newRoom;
  }

  public String getPosition() {
    return position;
  }

  public void setPosition(String newPosition) {
    position = newPosition;
  }

  public String getPiquetGSM() {
    return piquetGSM;
  }

  public void setPiquetGSM(String newPiquetGSM) {
    piquetGSM = newPiquetGSM;
  }

  public String getPiquetEmail() {
    return piquetEmail;
  }

  public void setPiquetEmail(String newPiquetEmail) {
    piquetEmail = newPiquetEmail;
  }

  public String toString() {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nALARM DEFINITION:");
    str_buf.append("\nFAULT FAMILY : ");
    str_buf.append(getFaultFamily());
    str_buf.append("\nFAULT MEMBER : ");
    str_buf.append(getFaultMember());
    str_buf.append("\nFAULT CODE : ");
    str_buf.append(getFaultCode());
    str_buf.append("\nSYSTEM NAME : ");
    str_buf.append(getSystemName());
    str_buf.append("\nIDENTIFIER : ");
    str_buf.append(getIdentifier());
    str_buf.append("\nPROBLEM DESCRIPTION : ");
    str_buf.append(getProblemDescription());
    str_buf.append("\nPRIORITY : ");
    str_buf.append(getPriority());
    str_buf.append("\nCAUSE : ");
    str_buf.append(getCause());
    str_buf.append("\nACTION : ");
    str_buf.append(getAction());
    str_buf.append("\nCONSEQUENCE : ");
    str_buf.append(getConsequence());
    str_buf.append("\nHELP URL : ");
    str_buf.append(getHelpURL());
    str_buf.append("\nINSTANT : ");
    str_buf.append(getInstant());
    str_buf.append("\nSOURCE : ");
    str_buf.append(getSourceName());
    str_buf.append("\nBUILDING : ");
    str_buf.append(getBuilding());
    str_buf.append("\nFLOOR : ");
    str_buf.append(getFloor());
    str_buf.append("\nROOM : ");
    str_buf.append(getRoom());
    str_buf.append("\nMNEMONIC : ");
    str_buf.append(getMnemonic());
    str_buf.append("\nPOSITION : ");
    str_buf.append(getPosition());
    str_buf.append("\nPIQUET GSM : ");
    str_buf.append(getPiquetGSM());
    str_buf.append("\nPIQUET EMAIL : ");
    str_buf.append(getPiquetEmail());
    str_buf.append("\nRESPONSIBLE : ");
    str_buf.append(getResponsiblePersonId());
    str_buf.append("\n");

    return str_buf.toString();
  }

  public String toShortString() {
    StringBuffer buffer = new StringBuffer();
    buffer.append(getFaultFamily());
    buffer.append(":");
    buffer.append(getFaultMember());
    buffer.append(":");
    buffer.append(getFaultCode());

    return buffer.toString();
  }

  public boolean equals(Object obj) {
    if ((obj == null) || (!(obj instanceof AlarmDefinition))) { return false; }
    AlarmDefinition def = (AlarmDefinition) obj;

    return getAlarmId().equals(def.getAlarmId());
  }

  public int hashCode() {
    return getAlarmId().hashCode();
  }
}
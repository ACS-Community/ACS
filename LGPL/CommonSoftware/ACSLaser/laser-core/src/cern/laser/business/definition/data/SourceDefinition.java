package cern.laser.business.definition.data;

import java.io.Serializable;

public class SourceDefinition implements Serializable {
  private String name;
  private String description;
  private Integer connectionTimeout;
  private Integer responsibleId;
  private String hostName;

  public SourceDefinition(String name) {
    setName(name);
  }

  public SourceDefinition(String name, String description, String hostName, Integer connectionTimeout,
      Integer responsibleId) {
    setName(name);
    setDescription(description);
    setHostName(hostName);
    setConnectionTimeout(connectionTimeout);
    setResponsiblePersonId(responsibleId);
  }

  public String getSourceId() {
    return name;
  }

  public String getName() {
    return name;
  }

  public void setName(String newName) {
    name = newName;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String newDescription) {
    description = newDescription;
  }

  public Integer getConnectionTimeout() {
    return connectionTimeout;
  }

  public void setConnectionTimeout(Integer newConnectionTimeout) {
    connectionTimeout = newConnectionTimeout;
  }

  public Integer getResponsiblePersonId() {
    return responsibleId;
  }

  public void setResponsiblePersonId(Integer newResponsibleId) {
    responsibleId = newResponsibleId;
  }

  public String getHostName() {
    return hostName;
  }

  public void setHostName(String newHostName) {
    hostName = newHostName;
  }

  public String toString() {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nALARM SOURCE DEFINITION:");
    str_buf.append("\nNAME : ");
    str_buf.append(getName());
    str_buf.append("\nDESCRIPTION : ");
    str_buf.append(getDescription());
    str_buf.append("\nHOST NAME : ");
    str_buf.append(getHostName());
    str_buf.append("\nCONNECTION TIMEOUT : ");
    str_buf.append(getConnectionTimeout());
    str_buf.append("\nRESPONSIBLE : ");
    str_buf.append(getResponsiblePersonId());
    str_buf.append("\n");

    return str_buf.toString();
  }

  public boolean equals(Object obj) {
    if ((obj == null) || (!(obj instanceof SourceDefinition))) { return false; }

    return getName().equals(((SourceDefinition) obj).getName());
  }

  public int hashCode() {
    return getName().hashCode();
  }
}
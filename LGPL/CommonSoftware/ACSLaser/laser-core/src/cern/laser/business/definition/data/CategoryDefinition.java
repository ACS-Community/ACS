package cern.laser.business.definition.data;

import java.io.Serializable;

public class CategoryDefinition implements Serializable {
  private String path;
  private String description;

  public CategoryDefinition(String path) {
    setPath(path);
  }

  public CategoryDefinition(String path, String description) {
    setPath(path);
    setDescription(description);
  }

//  public Integer getCategoryId() {
//    return (path == null ? null : new Integer(path.hashCode()));
//  }
//
  public String getParentPath() {
    if (path == null) { return null; }
    int last_dot_index = path.lastIndexOf(".");
    if (last_dot_index == -1) {
      return null;
    }
    
    String parent_path = path.substring(0, last_dot_index);
    return (parent_path == null ? null : parent_path);
  }

  public String getPath() {
    return path;
  }

  public void setPath(String newPath) {
    path = newPath;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String newDescription) {
    description = newDescription;
  }

  public String toString() {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nCATEGORY DEFINITION:");
    str_buf.append("\nPATH : ");
    str_buf.append(getPath());
    str_buf.append("\nDESCRIPTION : ");
    str_buf.append(getDescription());
    str_buf.append("\n");

    return str_buf.toString();
  }

  public boolean equals(Object obj) {
    if ((obj == null) || (!(obj instanceof CategoryDefinition))) { return false; }

    return getPath().equals(((CategoryDefinition) obj).getPath());
  }

  public int hashCode() {
    return getPath().hashCode();
  }
}
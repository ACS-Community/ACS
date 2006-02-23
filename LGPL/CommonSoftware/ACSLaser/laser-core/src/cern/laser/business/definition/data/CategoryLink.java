package cern.laser.business.definition.data;

import java.io.Serializable;

public class CategoryLink implements Serializable {
  private CategoryDefinition category;
  private AlarmDefinition alarm;

  public CategoryLink(CategoryDefinition category, AlarmDefinition alarm) {
    setCategory(category);
    setAlarm(alarm);
  }

  public AlarmDefinition getAlarm() {
    return alarm;
  }

  public void setAlarm(AlarmDefinition newAlarm) {
    alarm = newAlarm;
  }

  public CategoryDefinition getCategory() {
    return category;
  }

  public void setCategory(CategoryDefinition newCategory) {
    category = newCategory;
  }

  public String toString() {
    return "[" + category.getPath() + "," + alarm.getAlarmId() + "]";
  }

  public boolean equals(Object obj) {
    if (obj == null) { return false; }
    if (!(obj instanceof CategoryLink)) { return false; }
    CategoryLink link = (CategoryLink) obj;

    return (getCategory().equals(link.getCategory()) && getAlarm().equals(link.getAlarm()));
  }

  public int hashCode() {
    return toString().hashCode();
  }

}
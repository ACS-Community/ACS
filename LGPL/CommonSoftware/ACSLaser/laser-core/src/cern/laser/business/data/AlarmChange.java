package cern.laser.business.data;

import java.io.Serializable;

public class AlarmChange implements Serializable {
  private Alarm current;
  private Alarm previous;

  public AlarmChange(Alarm current, Alarm previous) {
    this.current = current;
    this.previous = previous;
  }

  public String getAlarmId() {
    return current.getAlarmId();
  }

  public Alarm getCurrent() {
    return current;
  }

  public void setCurrent(AlarmImpl newCurrent) {
    current = newCurrent;
  }

  public Alarm getPrevious() {
    return previous;
  }

  public void setPrevious(AlarmImpl newPrevious) {
    previous = newPrevious;
  }

  public int hashCode() {
    return getAlarmId().hashCode();
  }

  public boolean equals(Object obj) {
    if (obj == null) { return false; }
    if (!(obj instanceof AlarmChange)) { return false; }
    AlarmChange change = (AlarmChange) obj;

    return getAlarmId().equals(change.getAlarmId());
  }

  public String toString() {
    return "PREVIOUS : " + getPrevious().toString() + "\nCURRENT : " + getCurrent().toString();
  }

}
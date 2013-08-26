/*
 * $Id: BehaviourImpl.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 * 
 * $Date: 2006/09/25 08:52:36 $ $Revision: 1.2 $ $Author: acaproni $
 * 
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.console.impl;

import cern.laser.console.Behaviour;
import java.io.*;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class BehaviourImpl implements Behaviour, Serializable {
  private String dailyPrinter;
  private boolean dailyPrinting;
  private boolean alarmDistinguished;
  private boolean alarmAutoTerminated;
  private boolean alarmAutoKlaxon;
  private String klaxonVolume;
  private String[] columnsToDisplay;

  public BehaviourImpl() {
    dailyPrinter = "";
    dailyPrinting = false;
    alarmDistinguished = false;
    alarmAutoTerminated = false;
    alarmAutoKlaxon = false;
    klaxonVolume = Behaviour.SILENT_VOLUME;
    columnsToDisplay = new String[0];
  }

  public String getDailyPrinter() {
    return dailyPrinter;
  }

  public void setDailyPrinter(String newPrinter) {
    dailyPrinter = newPrinter;
  }

  public boolean isDailyPrinting() {
    return dailyPrinting;
  }

  public void setDailyPrinting(boolean newDailyPrinting) {
    dailyPrinting = newDailyPrinting;
  }

  public boolean isAlarmDistinguished() {
    return alarmDistinguished;
  }

  public void setAlarmDistinguished(boolean newAlarmDistinguished) {
    alarmDistinguished = newAlarmDistinguished;
  }

  public boolean isAlarmAutoTerminated() {
    return alarmAutoTerminated;
  }

  public void setAlarmAutoTerminated(boolean newAlarmAutoTerminated) {
    alarmAutoTerminated = newAlarmAutoTerminated;
  }

  public boolean isAlarmAutoKlaxon() {
    return alarmAutoKlaxon;
  }

  public void setAlarmAutoKlaxon(boolean newAlarmAutoKlaxon) {
    alarmAutoKlaxon = newAlarmAutoKlaxon;
  }

  public String getKlaxonVolume() {
    return klaxonVolume;
  }

  public void setKlaxonVolume(String newKlaxonVolume) {
    klaxonVolume = newKlaxonVolume;
  }

  public String[] getColumnsToDisplay() {
    return columnsToDisplay;
  }

  public void setColumnsToDisplay(String[] newColumnNames) {
    if (newColumnNames == null) { throw new IllegalArgumentException("argument can not be null"); }
    columnsToDisplay = (String[]) columnsToDisplay.clone();
  }

  public String toString() {
    StringBuffer str_buf = new StringBuffer();
    try {
      str_buf.append("\nCONSOLE BEHAVIOUR :");
      str_buf.append("\nCOLUMNS TO DISPLAY : ");
      for (int i = 0; i < columnsToDisplay.length; str_buf.append(columnsToDisplay[i++] + " "))
        ;
      str_buf.append("\nDAILY PRINTER : ");
      str_buf.append(getDailyPrinter());
      str_buf.append("\nDAILY PRINTING : ");
      str_buf.append(isDailyPrinting());
      str_buf.append("\nALARM DISTINGUISHED : ");
      str_buf.append(isAlarmDistinguished());
      str_buf.append("\nALARM AUTO TERMINATED : ");
      str_buf.append(isAlarmAutoTerminated());
      str_buf.append("\nAUTO KLAXON : ");
      str_buf.append(isAlarmAutoKlaxon());
      str_buf.append("\nKLAXON VOLUME : ");
      str_buf.append(getKlaxonVolume());
    } catch (Exception e) {
      str_buf.append("exception caught : " + e.getMessage());
    }

    return str_buf.toString();
  }

}
/*
 * $Id: Behaviour.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.3 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.console;

/** The alarm console behaviour. It defines the dynamic behaviour
 * of an alarm console display on alarm change reception, alarm rendering
 * and printing.
 * @author F.Calderini
 */
public interface Behaviour
{
  /** No klaxon. */
  public static final String SILENT_VOLUME = "SILENT";
  /** Low bell klaxon. */
  public static final String LOW_BELL_VOLUME = "LOW_BELL";
  /** High bell klaxon. */
  public static final String HIGH_BELL_VOLUME = "HIGH_BELL";
  /** Klaxon. */
  public static final String KLAXON_VOLUME = "KLAXON";
  /** Predefined klaxon volumes array. */
  public static final String[] volumes = {
    SILENT_VOLUME,
    LOW_BELL_VOLUME,
    HIGH_BELL_VOLUME,
    KLAXON_VOLUME
  };
  
  /** Get the printer name for alarm daily printing.
   * @return the daily printing printer name
   */    
  public String getDailyPrinter();
  /** Set the printer name for alarm daily printing.
   * @param newPrinter the daily printing printer name
   */    
  public void setDailyPrinter(String newPrinter);
  /** Check the alarm daily printing flag.
   * @return true iff daily printing is enabled.
   */    
  public boolean isDailyPrinting();
  /** Set the alarm daily printing flag.
   * @param newDailyPrinting if true daily printing is enabled.
   */    
  public void setDailyPrinting(boolean newDailyPrinting);

  /** Check the alarm distinction flag.
   * @return true iff alarm distinction is enabled. In alarm distinguished mode
   * new alarms are displayed with a special icon meaning that they have just
   * arrived on the screen.
   */    
  public boolean isAlarmDistinguished();
  /** Set the alarm distinction flag.
   * @param newAlarmDistinguished if true alarm distinction is enabled.
   */    
  public void setAlarmDistinguished(boolean newAlarmDistinguished);
  
  /** Check the alarm auto terminate flag.
   * @return true iff alarm auto terminate is enabled. In auto terminate mode
   * terminated alarms are automatically acknowlwdged and therefore removed from the
   * screen.
   */    
  public boolean isAlarmAutoTerminated();
  /** Set the alarm auto terminate flag.
   * @param newAlarmAutoTerminated if true alarm auto terminate is enabled.
   */    
  public void setAlarmAutoTerminated(boolean newAlarmAutoTerminated);

  /** Check the alarm auto klaxon flag.
   * @return true iff alarm auto klaxon is enabled. In auto klaxon mode
   * ALL new alarms automatically cause the klaxon. In reverse only alarms in
   * klaxon list will cause the klaxon.
   */    
  public boolean isAlarmAutoKlaxon();
  /** Set the alarm auto klaxon flag.
   * @param newAlarmAutoKlaxon if true alarm auto klaxon is enabled.
   */    
  public void setAlarmAutoKlaxon(boolean newAlarmAutoKlaxon);
  
  /** Get the alarm klaxon volume.
   * @return the alarm klaxon volume.
   */    
  public String getKlaxonVolume();
  /** Set the alarm klaxon volume.
   * @param newKlaxonVolume the alarm klaxon volume.
   */    
  public void setKlaxonVolume(String newKlaxonVolume);

  /** Get the column names to be displayed.
   * @return the column names.
   */    
  public String[] getColumnsToDisplay();
  /** Set newColumnNames
   * @param newColumnNames the column names.
   */    
  public void setColumnsToDisplay(String[] newColumnNames);

}
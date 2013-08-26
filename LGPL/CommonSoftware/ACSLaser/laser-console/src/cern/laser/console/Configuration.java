/*
 * $Id: Configuration.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.console;
import cern.laser.client.services.selection.Selection;

/** The alarm console overall configuration. It defines the dynamic behaviour
 * of an alarm console display, as well as the alarm category and filtering selection.
 * @author F.Calderini
 * @see cern.laser.client.services.selection.Selection
 * @see cern.laser.console.Behaviour
 */
public interface Configuration
{
  /** Get the configuration name.
   * @throws LaserConsoleException if the request could not be served.
   * @return the configuration name.
   */    
  public String getName() throws LaserConsoleException;
  /** Set the configuration name.
   * @throws LaserConsoleException if the request could not be served.
   * @param newName the configuration name.
   */    
  public void setName(String newName) throws LaserConsoleException;
  /** Check if the configuration is a default configuration.
   * @throws LaserConsoleException if the request could not be served.
   * @return true if it is a user's default configuration.
   */    
  public boolean isDefault() throws LaserConsoleException;
  
  /** Get the alarm selection.
   * @throws LaserConsoleException if the request could not be served.
   * @return the alarm selection.
   */    
  public Selection getSelection() throws LaserConsoleException;
  /** Set the alarm selection.
   * @throws LaserConsoleException if the request could not be served.
   * @param newSelection the alarm selection.
   */    
  public void setSelection(Selection newSelection) throws LaserConsoleException;
  
  /** Factory method.
   * @return a new alarm console behaviour instance.
   */    
  public Behaviour createBehaviour();
  /** Get the alarm console behaviour.
   * @return the alarm console behaviour.
   * @throws LaserConsoleException if the request could not be served.
   */
  public Behaviour getBehaviour() throws LaserConsoleException;
  /** Set the alarm console behaviour.
   * @param newBehaviour the alarm console behaviour.
   * @throws LaserConsoleException if the request could not be served.
   */    
  public void setBehaviour(Behaviour newBehaviour) throws LaserConsoleException;

  /** Get the masked alarms.
   * @return the masked alarms.
   * @throws LaserConsoleException if the request could not be served.
   */
  public CommentedAlarmMap getMasked() throws LaserConsoleException;

  /** Set the masked alarms.
   * @param newMasked the masked alarms.
   * @throws LaserConsoleException if the request could not be served.
   */
  public void setMasked(CommentedAlarmMap newMasked) throws LaserConsoleException;
  
  /** Get the inhibited alarms.
   * @return the inhibited alarms.
   * @throws LaserConsoleException if the request could not be served.
   */
  public CommentedAlarmMap getInhibited() throws LaserConsoleException;

  /** Set the inhibited alarms.
   * @param newInhibited the inhibited alarms.
   * @throws LaserConsoleException if the request could not be served.
   */
  public void setInhibited(CommentedAlarmMap newInhibited) throws LaserConsoleException;
  
  /** Get the highlighted alarms.
   * @return the highlighted alarms.
   * @throws LaserConsoleException if the request could not be served.
   */
  public CommentedAlarmMap getHighlighted() throws LaserConsoleException;

  /** Set the highlighted alarms.
   * @param  newHighlighted the highlighted alarms.
   * @throws LaserConsoleException if the request could not be served.
   */
  public void setHighlighted(CommentedAlarmMap newHighlighted) throws LaserConsoleException;
  
  /** Get the permanently highlighted alarms.
   * @return the permanently highlighted alarms.
   * @throws LaserConsoleException if the request could not be served.
   */
  public CommentedAlarmMap getAutoHighlighted() throws LaserConsoleException;

  /** Set the permanently highlighted alarms.
   * @param newAutoHighlighted the permanently highlighted alarms.
   * @throws LaserConsoleException if the request could not be served.
   */
  public void setAutoHighlighted(CommentedAlarmMap newAutoHighlighted) throws LaserConsoleException;

  /** Get the permanently klaxoned alarms.
   * @return the permanently klaxoned alarms.
   * @throws LaserConsoleException if the request could not be served.
   */
  public CommentedAlarmMap getAutoKlaxoned() throws LaserConsoleException;

  /** Set the permanently klaxoned alarms.
   * @param newAutoKlaxoned the permanently klaxoned alarms.
   * @throws LaserConsoleException if the request could not be served.
   */
  public void setAutoKlaxoned(CommentedAlarmMap newAutoKlaxoned) throws LaserConsoleException;

  /** Get the acknowledged alarms.
   * @return the acknowledged alarms.
   * @throws LaserConsoleException if the request could not be served.
   */
  public CommentedAlarmMap getAcknowledged() throws LaserConsoleException;

  /** Set the acknowledged alarms.
   * @param newAcknowledged the acknowledged alarms.
   * @throws LaserConsoleException if the request could not be served.
   */
  public void setAcknowledged(CommentedAlarmMap newAcknowledged) throws LaserConsoleException;
  
  /** Get the new alarms.
   * @return the new alarms.
   * @throws LaserConsoleException if the request could not be served.
   */
  public CommentedAlarmMap getNewIndicator() throws LaserConsoleException;

  /** Set the new alarms.
   * @param newAcknowledged the new alarms.
   * @throws LaserConsoleException if the request could not be served.
   */
  public void setNewIndicator(CommentedAlarmMap newNewIndicator) throws LaserConsoleException;
  
  public Boolean getActiveListFont() throws LaserConsoleException;
  
  public void setActiveListFont(Boolean isActiveListFont) throws LaserConsoleException;
}
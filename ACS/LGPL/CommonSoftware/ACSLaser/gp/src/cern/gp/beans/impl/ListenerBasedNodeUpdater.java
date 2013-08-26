/*
 * $Id: ListenerBasedNodeUpdater.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.beans.impl;

/**
 * <i><font size="-1" color="#FF0000">**Experimental : for internal use only** </font></i>
 * Provides support for implementing a <code>NodeUpdater</code>.
 * <p>
 * A bean that implements the <code>NodeUpdaterProvider</code> interface, in
 * order to dynamically update the GUI, will typically use this class as the 
 * <code>NodeUpdater</code> that is returned.
 * </p>
 * <p>
 * The setter methods should be used to set the initial values and to notify of a new
 * value.
 * </p>
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 *
 */
abstract class ListenerBasedNodeUpdater implements NodeUpdater {

  private ListenerList eventListeners;
  
  //
  // -- CONSTRUCTORS -----------------------------------------------
  //

  /**
   * Creates a new <code>ListenerBasedNodeUpdater</code>
   */
  protected ListenerBasedNodeUpdater() {
    eventListeners = new WeakReferenceListenerList();
  }

  //
  // -- PUBLIC METHODS -----------------------------------------------
  //

  //
  // -- implements NodeUpdater -----------------------------------------------
  //
  
  public void addNodeUpdaterListener(NodeUpdaterListener listener) {
    synchronized (eventListeners) {
      if (!eventListeners.contains(listener)) {
        eventListeners.add(listener);
      }
    }
  }

  public void removeNodeUpdaterListener(NodeUpdaterListener listener) {
    synchronized (eventListeners) {
      eventListeners.remove(listener);
    }
  }


  //
  // -- PROTECTED METHODS -----------------------------------------------
  //

  /**
   * Returns true is this support has at least one registered listener
   * @return true is this support has at least one registered listener
   */
  protected final boolean hasListeners() {
    return !eventListeners.isEmpty();
  }

  protected final void fireNameChange(String newName) {
    if (! hasListeners()) return;
    synchronized (eventListeners) {
      java.util.Iterator iterator = eventListeners.iterator();
      while (iterator.hasNext()) {
        ((NodeUpdaterListener) iterator.next()).nameChanged(newName);
      }
    }
  }
  
  protected final void fireDisplayNameChange(String newDisplayName) {
    if (! hasListeners()) return;
    synchronized (eventListeners) {
      java.util.Iterator iterator = eventListeners.iterator();
      while (iterator.hasNext()) {
        ((NodeUpdaterListener) iterator.next()).displayNameChanged(newDisplayName);
      }
    }
  }
  
  protected final void fireShortDescriptionChange(String newShortDescription) {
    if (! hasListeners()) return;
    synchronized (eventListeners) {
      java.util.Iterator iterator = eventListeners.iterator();
      while (iterator.hasNext()) {
        ((NodeUpdaterListener) iterator.next()).shortDescriptionChanged(newShortDescription);
      }
    }
  }

  protected final void fireNodeDefaultActionChange(String newDefaultAction) {
    if (! hasListeners()) return;
    synchronized (eventListeners) {
      java.util.Iterator iterator = eventListeners.iterator();
      while (iterator.hasNext()) {
        ((NodeUpdaterListener) iterator.next()).nodeDefaultActionChanged(newDefaultAction);
      }
    }
  }  
  
  protected final void fireNodeIconChange(java.awt.Image newIcon) {
    if (! hasListeners()) return;
    synchronized (eventListeners) {
      java.util.Iterator iterator = eventListeners.iterator();
      while (iterator.hasNext()) {
        ((NodeUpdaterListener) iterator.next()).nodeIconChanged(newIcon);
      }
    }
  }  
  

  //
  // -- PRIVATE METHODS -----------------------------------------------
  //

  //
  // -- INNER CLASSES -----------------------------------------------
  //

  /**
   * A class implementing this interface provide a minimum set of methods
   * to support the addition and removal of listener.
   * @author Lionel Mestre
   */
  private interface ListenerList {

    /**
     * Returns true if no listener is in the list.
     * @return true if no listener is in the list.
     */
    public boolean isEmpty();

    /**
     * Returns the number of listeners in the list.
     * @return the number of listeners in the list.
     */
    public int size();

    /**
     * Returns true if <code>listener</code> is a listener contained in the list.
     * @return true if <code>listener</code> is a listener contained in the list.
     */
    public boolean contains(NodeUpdaterListener listener);

    /**
     * Adds the given listener
     * @param <code>listener</code> the listener to add in the list
     * @return true if the listener has been added.
     */
    public boolean add(NodeUpdaterListener listener);

    /**
     * Removes the given listener
     * @param <code>listener</code> the listener to remove from the list
     * @return true if the listener has been removed.
     */
    public boolean remove(NodeUpdaterListener listener);

    /**
     * Returns an iterator on the listeners of the list
     * @return an iterator on the listeners of the list
     */
    public java.util.Iterator iterator();

  }

  /**
   * Implements a simple list of listeners backed by an <code>ArrayList</code>.
   * @author Lionel Mestre
   */
  private class PlainListenerList implements ListenerList {

    protected java.util.ArrayList list;

    public PlainListenerList() {
      list = new java.util.ArrayList();
    }

    public boolean isEmpty() {
      return list.isEmpty();
    }

    public int size() {
      return list.size();
    }

    public boolean contains(NodeUpdaterListener listener) {
      return list.contains(listener);
    }

    public boolean add(NodeUpdaterListener listener) {
      return list.add(listener);
    }

    public boolean remove(NodeUpdaterListener listener) {
      return list.remove(listener);
    }

    public java.util.Iterator iterator() {
      return list.iterator();
    }

  }

  /**
   * Implements a list of listeners in which the reference to one listener
   * is a WeakReference allowing that listener to be garbage collected if
   * it is no more used.
   * @author Lionel Mestre
   */
  private class WeakReferenceListenerList extends PlainListenerList {

    public WeakReferenceListenerList() {
    }

    public boolean contains(NodeUpdaterListener listener) {
      java.util.Iterator iterator = iterator();
      while (iterator.hasNext()) {
        if (iterator.next() == listener)
          return true;
      }
      return false;
    }

    public boolean add(NodeUpdaterListener listener) {
      return list.add(new java.lang.ref.WeakReference(listener));
    }

    public boolean remove(NodeUpdaterListener listener) {
      java.util.Iterator iterator = iterator();
      while (iterator.hasNext()) {
        if (iterator.next() == listener) {
          iterator.remove();
          return true;
        }
      }
      return false;
    }

    public java.util.Iterator iterator() {
      return new WeakReferenceIterator(list.iterator());
    }

  }

  /**
   * Implements an <code>Iterator</code> on a list containing
   * <code>WeakReference</code>s and return the referenced object.
   * The iterator automatically removed from the list the objects
   * that have been garbage collected.
   *
   * @author Lionel Mestre
   *
   */
  private class WeakReferenceIterator implements java.util.Iterator {

    private java.util.Iterator iterator;
    private Object nextObject;

    public WeakReferenceIterator(java.util.Iterator iterator) {
      this.iterator = iterator;
      nextObject = getNextObject();
    }

    public boolean hasNext() {
      return nextObject != null;
    }

    public Object next() {
      Object result = nextObject;
      nextObject = getNextObject();
      return result;
    }

    public void remove() {
      iterator.remove();
    }

    private Object getNextObject() {
      while (iterator.hasNext()) {
        java.lang.ref.WeakReference ref = (java.lang.ref.WeakReference) iterator.next();
        Object target = ref.get();
        if (target == null) {
          // object has been removed
          iterator.remove();
        } else {
          return target;
        }
      }
      return null;
    }
  } // end inner class WeakReferenceIterator

}


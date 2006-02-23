package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (6/26/00 20:07:44)
 * @author: 
 */
public class PropertyChangeSupportable {
	protected java.beans.PropertyChangeSupport propertyListeners = new java.beans.PropertyChangeSupport(this);
/**
 * BeamAnalizatorBean constructor comment.
 */
public PropertyChangeSupportable() {
	super();
}
public void addPropertyChangeListener(java.beans.PropertyChangeListener listener) {
	propertyListeners.addPropertyChangeListener(listener);
}
public void addPropertyChangeListener(String propertyName, java.beans.PropertyChangeListener listener) {
	propertyListeners.addPropertyChangeListener(propertyName,listener);
}
/**
 * Insert the method's description here.
 * Creation date: (7/21/00 16:45:55)
 * @param event java.beans.PropertyChangeEvent
 */
protected void firePropertyChange(java.beans.PropertyChangeEvent event) {
	propertyListeners.firePropertyChange(event);
}
/**
 * Insert the method's description here.
 * Creation date: (7/21/00 16:45:55)
 * @param event java.beans.PropertyChangeEvent
 */
protected void firePropertyChange(String propertyName) {
	propertyListeners.firePropertyChange(propertyName, 0, 1);
}
public void removePropertyChangeListener(java.beans.PropertyChangeListener listener) {
	propertyListeners.removePropertyChangeListener(listener);
}
public void removePropertyChangeListener(String propertyName, java.beans.PropertyChangeListener listener) {
	propertyListeners.removePropertyChangeListener(propertyName,listener);
}
}

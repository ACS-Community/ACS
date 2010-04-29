/**
 * 
 */
package alma.acs.tmcdb.translator;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

/**
 * @author rtobar
 *
 */
public class TmcdbObject {

	protected PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(
			this);

	public void addPropertyChangeListener(String propertyName,
			PropertyChangeListener listener) {
		propertyChangeSupport.addPropertyChangeListener(propertyName, listener);
	}

	public void removePropertyChangeListener(PropertyChangeListener listener) {
		propertyChangeSupport.removePropertyChangeListener(listener);
	}

}

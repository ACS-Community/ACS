/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2002
 * Copyright by ESO (in the framework of the ALMA collaboration)
 * and Cosylab 2002, All rights reserved
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

package alma.ACS.jbaci;

import java.util.ArrayList;
import java.util.Iterator;

/**
 * Convenience implementation of <code>DataAccess</code> - implementing listener (de)registration.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public abstract class DataAccessSupport implements DataAccess {

	/**
	 * List of registered listeners.
	 */
	protected ArrayList<ValueChangeListener> listeners = new ArrayList<ValueChangeListener>();

	/**
	 * Notifies about value change - internal method.
	 * @param oldValue	old value.
	 * @param newValue	new value.
	 */
	protected void notify(Object oldValue, Object newValue)
	{
		if (listeners.size() == 0)
			return;
			
		synchronized (listeners)
		{
			Iterator<ValueChangeListener> iter = listeners.iterator();
			while (iter.hasNext())
			{
				try
				{
					iter.next().valueChanged(this, oldValue, newValue);
				}
				catch (Throwable th)
				{
					// TODO or even log here
					th.printStackTrace();
				}
			}
		}
	}

	/**
	 * @see alma.ACS.jbaci.DataAccess#addValueChangeListener(alma.ACS.jbaci.DataAccess.ValueChangeListener)
	 */
	public void addValueChangeListener(ValueChangeListener listener)
		throws OnChangeNotSupportedException {
		synchronized (listeners)
		{
			if (!listeners.contains(listener))
				listeners.add(listener);
		}
	}

	/**
	 * @see alma.ACS.jbaci.DataAccess#removeValueChangeListener(alma.ACS.jbaci.DataAccess.ValueChangeListener)
	 */
	public void removeValueChangeListener(ValueChangeListener listener) {
		synchronized (listeners)
		{
			listeners.remove(listener);
		}
	}

}

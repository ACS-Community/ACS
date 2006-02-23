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

import alma.ACSErr.CompletionHolder;
import alma.acs.exceptions.AcsJException;

/**
 * Data access interface.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public interface DataAccess {
	
	/**
	 * Value change listener interface.
	 */
	public interface ValueChangeListener
	{
		/**
		 * Method called on value change.
		 * NOTE: method should complete as soon as possible!
		 * @param source	<code>DataAccess</code> instance firing this event.
		 * @param oldValue	old value.
		 * @param newValue	new value.
		 */
		public void valueChanged(DataAccess source, Object oldValue, Object newValue);
	}

	/**
	 * Exeption thrown if <code>DataAccess</code> does not support on-change notifications.
	 * This exeption provides <code>recommendedPoolTime()</code> method to recommend pool time,
	 * so that listener could simulate on-change notification using pooling mechanism.
	 */
	public class OnChangeNotSupportedException extends Exception
	{
		/**
		 * Recommended pool time in ms.
		 */
		protected long poolTime;
		
		/**
		 * Constructor.
		 * @param poolTime	recommended pool time, can be 0.
		 */
		public OnChangeNotSupportedException(long poolTime) {
			super();
			this.poolTime = poolTime;
		}

		/**
		 * Recommended pool time in ms, can be 0.
		 */
		public long recommendedPoolTime()
		{
			return poolTime;
		}
	}

	/**
	 * Add value change listener.
	 * @param listener	listener to be notified on every value change.
	 * @throws NotSupportedException	throws id this <code>DataAccess</code> does not support
	 * 									on-change notifications - exeptions provides recommended pool time
	 * 									property.
	 * @see NotSupportedException
	 */
	public void addValueChangeListener(ValueChangeListener listener) throws OnChangeNotSupportedException;
	
	/**
	 * Remove value change listener.
	 * @param listener	listener to be removed.
	 */
	public void removeValueChangeListener(ValueChangeListener listener);

	/**
	 * Retrieve value or throw <code>AcsJExcption</code> in case of failure.
	 * @param completionHolder	completion holder. If <code>completionHolder.value</code>
	 * 						is left to <code>null</code> (by this method), caller is responsible to
	 * 						create a no-error completion with current timestamp later.
	 * 						Use <code>alma.ACS.jbaci.CompletionUtil</code> class to generate no-error completion.
	 * @return read value, non-<code>null</code>.
	 * @throws	ACS exception if case of failure.
	 * @see alma.ACS.jbaci.CompletionUtil#generateNoErrorCompletion()
	 */
	public Object get(CompletionHolder completionHolder) throws AcsJException;
	
	
	/**
	 * Flag indicating if property should initialize value (to default value) when initializing. 
	 * @return flag indicating if property should initialize value (to default value) when initializing.
	 */
	public boolean initializeValue();
	
	/**
	 * Set value or throw <code>AcsJExcption</code> in case of failure.
	 * @param value	value to be set, non-<code>null</code>.
	 * @param completionHolder	completion holder. If <code>completionHolder.value</code>
	 * 						is left to <code>null</code> (by this method), caller is responsible to
	 * 						create a no-error completion with current timestamp later.
	 * 						Use <code>alma.ACS.jbaci.CompletionUtil</code> class to generate no-error completion.
	 * @throws	ACS exception if case of failure.
	 */
	public void set(Object value, CompletionHolder completion) throws AcsJException;
	
}

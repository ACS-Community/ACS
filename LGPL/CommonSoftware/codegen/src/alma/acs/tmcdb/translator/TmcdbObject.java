/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.tmcdb.translator;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.Set;

/**
 * Base class for all code-generated TMCDB tables java mappings (pojo objects)
 * 
 * This class provides (at the time) two main important features:
 * <ul>
 * 
 * 	<li> It adds domain self-contained publisher/subscriber support. With this, any object 
 *       that implements {@link java.beans.PropertyChangeListener} can subscribe to
 *       changes in the internal object's properties and listen to them. Note though that this
 *       feature is only possible when classes extending <code>TmcdbObject</code> fire property
 *       changes on their property setters, which is the case of the code-generated classes.
 *
 *  <li> It gives the chance to the user to select whether the <code>equals()</code> and
 *       <code>hashCode()</code> methods should use the default java <code>Object</code>
 *       implementations, or the <code>equalsContent()</code> and <code>hashCodeContent()</code>
 *       ones. These new methods should be reimplemented by classes extending <code>TmcdbObject</code>,
 *       using business logic, otherwise the methods declared in this class will be used,
 *       which call to {@link Object#equals(Object)} and {@link Object#hashCode()} anyway.
 *       In the case of code-generated classes, these methods are created, if it is the case,
 *       with the columns present in the "GENERATED FROM" statement of the model. For further
 *       details, refer to the grammar definition for the TMCDB models.
 *       
 *       <p>By default, the <code>Object</code> methods will be used. This can be changed by the user
 *       at any time by calling {@link #setUseContentEqualsAndHashCode(boolean)}.
 *
 *       <p><strong>NOTE</strong>: Use this with care. This option is available for it eases life
 *       to some programs, but it might lead to hard-to-recognize problems. Please refer to
 *       {@link http://www.artima.com/lejava/articles/equality.html} and to the {@link Set} class
 *       documentation for further details 
 *  
 * </ul>
 * 
 * 
 * @author rtobar
 * @since ACS 8.2
 *
 */
public class TmcdbObject {

	protected PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(
			this);

	protected boolean useContentEqualsAndHashCode = false;
	
	public void addPropertyChangeListener(String propertyName, PropertyChangeListener listener) {
		propertyChangeSupport.addPropertyChangeListener(propertyName, listener);
	}

	public void removePropertyChangeListener(String propertyName, PropertyChangeListener listener) {
		propertyChangeSupport.removePropertyChangeListener(propertyName, listener);
	}

	public boolean getUseContentEqualsAndHashCode() {
		return useContentEqualsAndHashCode;
	}

	public void setUseContentEqualsAndHashCode(boolean u) {
		useContentEqualsAndHashCode = u;
	}

	public boolean equalsContent(Object o) {
		return super.equals(o);
	}

	public int hashCodeContent() {
		return super.hashCode();
	}

	public boolean equals(Object o) {
		if( useContentEqualsAndHashCode )
			return equalsContent(o);
		else
			return super.equals(o);
	}

	public int hashCode() {
		if( useContentEqualsAndHashCode )
			return hashCodeContent();
		else
			return super.hashCode();
	}
}

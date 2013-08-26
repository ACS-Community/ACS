/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acsplugins.alarmsystem.gui.specialalarm;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.Properties;
import java.util.Vector;

import cern.laser.client.data.Alarm;
import cern.laser.client.data.Location;
import cern.laser.client.data.ResponsiblePerson;
import cern.laser.client.data.Source;
import cern.laser.client.data.Status;
import cern.laser.client.data.Triplet;

/**
 * An implementation of the Alarm to allow sending alarms to the
 * panel through the addSpecialAlarm() method.
 * 
 * @author acaproni
 * @since ACS 8.1.0
 */
public class SpecialAlarm implements Alarm {
	
	/**
	 * The triplet
	 */
	private final PanelTriplet triplet;
	
	/**
	 * The status of the alarm
	 */
	private final PanelStatus status;
	
	/**
	 * The action to fix the problem
	 */
	private final String action;
	
	/**
	 * Description of the problem
	 */
	private final String description;
	
	/**
	 * The cause that generate the problem
	 */
	private final String cause;
	
	/*
	 * Conequences of the problem
	 */
	private final String consequences;
	
	/**
	 * The priority, or level, of the alarm.
	 * <P>
	 * <code>priority</code> is in [0..3] being 0 the highest priority.
	 */
	private final int priority;
	
	/**
	 * The RUL with the detailed documentation of the alarm
	 */
	private final URL url;
	
	/**
	 * Email address of the responsible for the alarm.
	 */
	private final String email;
	
	
	
	/**
	 * Build a SpecialAlarm filling the fields as requested by the
	 * alarm panel and alarm system conventions.
	 * <P>
	 * Refers to the properties of this class for a full
	 * description of the parameters.
	 * 
	 * @param family Fault family: can't be <code>null</code> nor empty.
	 * @param component Fault member (or component): can't be <code>null</code> nor empty.
	 * @param code Fault code
	 * @param active <code>true</code> if the alarm is active
	 * @param action Can't be <code>null</code> nor empty.
	 * @param description Can't be <code>null</code> nor empty.
	 * @param cause
	 * @param consequences
	 * @param priority Belongs to [0..3]. 0 is the highest priority.
	 * @param url Can be <code>null</code> or empty
	 * @param email
	 * @param Properties props
	 * @throws MalformedURLException If the passed url is malformed. 
	 * 							This exception is not thrown when <code>url</code> 
	 * 							is null or empty.
	 */
	public SpecialAlarm(
			String family, 
			String component, 
			int code,
			boolean active, 
			String action, 
			String description, 
			String cause,
			String consequences, 
			int priority, 
			String url, 
			String email,
			Properties props) 
	throws MalformedURLException {
		if (family==null || family.isEmpty()) {
			throw new IllegalArgumentException("The family can't be null nor empty");
		}
		if (component==null || component.isEmpty()) {
			throw new IllegalArgumentException("The component can't be null nor empty");
		}
		this.triplet = new PanelTriplet(family, component, code);
		
		if (action==null || action.isEmpty()) {
			throw new IllegalArgumentException("The action can't be null nor empty");
		}
		this.action = action;
		
		if (description==null || description.isEmpty()) {
			throw new IllegalArgumentException("The description can't be null nor empty");
		}
		this.description = description;

		if (cause==null) {
			cause="";
		}
		this.cause = cause;
		
		if (consequences==null) {
			consequences="";
		}
		this.consequences = consequences;
		
		if (priority<0 || priority>3) {
			throw new IllegalArgumentException("Priority must belong tp [0..3]");
		}
		this.priority = priority;
		
		URL docURL=null;
		if (url!=null && !url.isEmpty()) {
			docURL=new URL(url);
		}
		this.url = docURL;
		
		if (email==null) {
			email="";
		}
		this.email = email;
		
		// Create the Status
		status = new PanelStatus(active, props);
	}

	/**
	 * @see cern.laser.client.data.Alarm#getAction()
	 */
	@Override
	public String getAction() {
		return action;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getAlarmId()
	 */
	@Override
	public String getAlarmId() {
		return triplet.FF+":"+triplet.FM+":"+triplet.FC;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getCategories()
	 */
	@Override
	public Collection getCategories() {
		return new Vector();
	}

	/**
	 * @see cern.laser.client.data.Alarm#getCause()
	 */
	@Override
	public String getCause() {
		return cause;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getConsequence()
	 */
	@Override
	public String getConsequence() {
		return consequences;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getHelpURL()
	 */
	@Override
	public URL getHelpURL() {
		return url;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getIdentifier()
	 */
	@Override
	public String getIdentifier() {
		return null;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getLocation()
	 */
	@Override
	public Location getLocation() {
		return null;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getPiquetEmail()
	 */
	@Override
	public String getPiquetEmail() {
		return email;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getPiquetGSM()
	 */
	@Override
	public String getPiquetGSM() {
		return null;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getPriority()
	 */
	@Override
	public Integer getPriority() {
		return Integer.valueOf(priority);
	}

	/**
	 * @see cern.laser.client.data.Alarm#getProblemDescription()
	 */
	@Override
	public String getProblemDescription() {
		return description;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getResponsiblePerson()
	 */
	@Override
	public ResponsiblePerson getResponsiblePerson() {
		return null;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getSource()
	 */
	@Override
	public Source getSource() {
		return null;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getStatus()
	 */
	@Override
	public Status getStatus() {
		return status;
	}
	
	/**
	 * Set the status active/inactive of the alarm
	 */
	public void setStatus(boolean active) {
		status.setStatus(active);
	}
	
	/**
	 * Set the user properties of the alarm
	 * 
	 * @param newPros The new properties
	 */
	public void setUserProperties(Properties newPros) {
		status.setUserProps(newPros);
	}

	/**
	 * @see cern.laser.client.data.Alarm#getSystemName()
	 */
	@Override
	public String getSystemName() {
		return "";
	}

	/**
	 * @see cern.laser.client.data.Alarm#getTriplet()
	 */
	@Override
	public Triplet getTriplet() {
		return triplet;
	}

	/**
	 * @see cern.laser.client.data.Alarm#isInstant()
	 */
	@Override
	public boolean isInstant() {
		return false;
	}

	/**
	 * @see cern.laser.client.data.Alarm#isMultiplicityChild()
	 */
	@Override
	public boolean isMultiplicityChild() {
		return false;
	}

	/**
	 * @see cern.laser.client.data.Alarm#isMultiplicityParent()
	 */
	@Override
	public boolean isMultiplicityParent() {
		return false;
	}

	/**
	 * @see cern.laser.client.data.Alarm#isNodeChild()
	 */
	@Override
	public boolean isNodeChild() {
		return false;
	}

	/**
	 * @see cern.laser.client.data.Alarm#isNodeParent()
	 */
	@Override
	public boolean isNodeParent() {
		return false;
	}
	
	public Object clone() throws CloneNotSupportedException {
		throw new CloneNotSupportedException();
	}
}

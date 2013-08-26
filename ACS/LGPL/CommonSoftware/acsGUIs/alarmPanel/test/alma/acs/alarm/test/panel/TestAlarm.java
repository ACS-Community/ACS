/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
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
package alma.acs.alarm.test.panel;

import java.net.URL;
import java.util.Collection;
import java.util.Random;

import cern.laser.client.data.Alarm;
import cern.laser.client.data.Location;
import cern.laser.client.data.ResponsiblePerson;
import cern.laser.client.data.Source;
import cern.laser.client.data.Status;
import cern.laser.client.data.Triplet;

/**
 * Interface for sending alarms to the alarm container.
 * 
 * @author acaproni
 *
 */
public class TestAlarm implements Alarm {
	
	/**
	 * The ID used to distinguish alarms in the static generator
	 * methods.
	 */
	public static int alarm_generator_id=0;
	
	/**
	 * Random generator
	 */
	private static Random rnd = new Random(System.currentTimeMillis());
	
	/**
	 * The alarm ID
	 */
	private final String id;
	
	/**
	 * <code>true</code> if this node is a child node
	 */
	private boolean child;
	
	/**
	 * <code>true</code> if this node is a parent node
	 */
	private boolean parent;
	
	/**
	 * The state of the alarm
	 */
	private final TestAlarmStatus status;
	
	/**
	 * The priority
	 */
	private int priority;
	
	/**
	 * Constructor
	 * 
	 */
	public TestAlarm(
			String id, 
			boolean child, 
			boolean parent, 
			boolean active, 
			boolean masked, 
			boolean reduced) {
		if (id==null || id.isEmpty()) {
			throw new IllegalArgumentException("Invalid alarm ID");
		}
		this.id=id;
		status = new TestAlarmStatus(active,masked,reduced);
		this.child=child;
		this.parent=parent;
		this.priority=Math.abs(rnd.nextInt())%4;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getAction()
	 */
	@Override
	public String getAction() {
		return null;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getAlarmId()
	 */
	@Override
	public String getAlarmId() {
		return id;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getCategories()
	 */
	@Override
	public Collection getCategories() {
		return null;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getCause()
	 */
	@Override
	public String getCause() {
		return null;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getConsequence()
	 */
	@Override
	public String getConsequence() {
		return null;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getHelpURL()
	 */
	@Override
	public URL getHelpURL() {
		return null;
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
		return null;
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
		return priority;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getProblemDescription()
	 */
	@Override
	public String getProblemDescription() {
		return null;
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
	 * @see cern.laser.client.data.Alarm#getSystemName()
	 */
	@Override
	public String getSystemName() {
		return null;
	}

	/**
	 * @see cern.laser.client.data.Alarm#getTriplet()
	 */
	@Override
	public Triplet getTriplet() {
		return null;
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
		return child;
	}

	/**
	 * @see cern.laser.client.data.Alarm#isNodeParent()
	 */
	@Override
	public boolean isNodeParent() {
		return parent;
	}
	
	/**
	 * @see cern.laser.client.data.Alarm#isNodeParent()
	 */
	@Override
	public Object clone() throws CloneNotSupportedException {
		return new TestAlarm(id,child,parent,status.isActive(),status.isMasked(),status.isReduced());
	}
	
	/**
	 * Helper method returning a random alarm.
	 * <P>
	 * The triplet of each alarm is as follows:
	 * <UL>
	 * 	<LI><B>FF</B>: RND
	 * 	<LI><B>FM</B>: fm+integer
	 * <LI><B>FC</B>: 1 
	 * </ul>
	 * The fault member is generated by adding an integer to the passed string, 
	 * i.e. the FM is a unique identifier.
	 * 
	 * @param fm The fault member of the alarm
	 * 
	 * @return The randomly generated alarm
	 */
	public static TestAlarm generateRndAlarm(String fm) {
		String alarmIdStr = "RND:"+fm+alarm_generator_id+":1";
		alarm_generator_id++;
		return new TestAlarm(
				alarmIdStr,
				rnd.nextBoolean(),
				rnd.nextBoolean(),
				rnd.nextBoolean(),
				rnd.nextBoolean(),
				rnd.nextBoolean());
	}
}

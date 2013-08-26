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
package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "esDataEvent"
 *	@author JacORB IDL compiler 
 */

public final class esDataEvent
	implements org.omg.CORBA.portable.IDLEntity
{
	public esDataEvent(){}
	public tdem.TDEM_TOPICS.sensorSpace setpoint;
	public tdem.TDEM_TOPICS.sensorSpace readback;
	public int key;
	public long timestamp;
	public esDataEvent(tdem.TDEM_TOPICS.sensorSpace setpoint, tdem.TDEM_TOPICS.sensorSpace readback, int key, long timestamp)
	{
		this.setpoint = setpoint;
		this.readback = readback;
		this.key = key;
		this.timestamp = timestamp;
	}
}

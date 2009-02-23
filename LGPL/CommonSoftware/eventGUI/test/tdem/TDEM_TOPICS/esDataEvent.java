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

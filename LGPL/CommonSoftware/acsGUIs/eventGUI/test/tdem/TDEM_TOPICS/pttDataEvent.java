package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "pttDataEvent"
 *	@author JacORB IDL compiler 
 */

public final class pttDataEvent
	implements org.omg.CORBA.portable.IDLEntity
{
	public pttDataEvent(){}
	public tdem.TDEM_TOPICS.actuatorSpace setpoint;
	public tdem.TDEM_TOPICS.actuatorSpace readback;
	public int key;
	public long timestamp;
	public pttDataEvent(tdem.TDEM_TOPICS.actuatorSpace setpoint, tdem.TDEM_TOPICS.actuatorSpace readback, int key, long timestamp)
	{
		this.setpoint = setpoint;
		this.readback = readback;
		this.key = key;
		this.timestamp = timestamp;
	}
}

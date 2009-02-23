package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "pttOffsetEvent"
 *	@author JacORB IDL compiler 
 */

public final class pttOffsetEvent
	implements org.omg.CORBA.portable.IDLEntity
{
	public pttOffsetEvent(){}
	public tdem.TDEM_TOPICS.actuatorSpace value;
	public int key;
	public long timestamp;
	public pttOffsetEvent(tdem.TDEM_TOPICS.actuatorSpace value, int key, long timestamp)
	{
		this.value = value;
		this.key = key;
		this.timestamp = timestamp;
	}
}

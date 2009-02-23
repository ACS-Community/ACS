package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "pointingOffsetEvent"
 *	@author JacORB IDL compiler 
 */

public final class pointingOffsetEvent
	implements org.omg.CORBA.portable.IDLEntity
{
	public pointingOffsetEvent(){}
	public double alt;
	public double az;
	public int key;
	public long timestamp;
	public pointingOffsetEvent(double alt, double az, int key, long timestamp)
	{
		this.alt = alt;
		this.az = az;
		this.key = key;
		this.timestamp = timestamp;
	}
}

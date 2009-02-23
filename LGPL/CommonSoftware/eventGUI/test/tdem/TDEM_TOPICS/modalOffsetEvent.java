package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "modalOffsetEvent"
 *	@author JacORB IDL compiler 
 */

public final class modalOffsetEvent
	implements org.omg.CORBA.portable.IDLEntity
{
	public modalOffsetEvent(){}
	public tdem.TDEM_TOPICS.modalSpace value;
	public int key;
	public long timestamp;
	public modalOffsetEvent(tdem.TDEM_TOPICS.modalSpace value, int key, long timestamp)
	{
		this.value = value;
		this.key = key;
		this.timestamp = timestamp;
	}
}

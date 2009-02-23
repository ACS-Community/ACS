package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "interlockStatusEvent"
 *	@author JacORB IDL compiler 
 */

public final class interlockStatusEvent
	implements org.omg.CORBA.portable.IDLEntity
{
	public interlockStatusEvent(){}
	public boolean motionStop1;
	public boolean motionStop2;
	public boolean emergencyStop1;
	public int key;
	public long timestamp;
	public interlockStatusEvent(boolean motionStop1, boolean motionStop2, boolean emergencyStop1, int key, long timestamp)
	{
		this.motionStop1 = motionStop1;
		this.motionStop2 = motionStop2;
		this.emergencyStop1 = emergencyStop1;
		this.key = key;
		this.timestamp = timestamp;
	}
}

package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "axisPositionEvent"
 *	@author JacORB IDL compiler 
 */

public final class axisPositionEvent
	implements org.omg.CORBA.portable.IDLEntity
{
	public axisPositionEvent(){}
	public double ref;
	public double act;
	public int key;
	public long timestamp;
	public double actAz;
	public double actEl;
	public double actRA;
	public double actDec;
	public double trackingError;
	public axisPositionEvent(double ref, double act, int key, long timestamp, double actAz, double actEl, double actRA, double actDec, double trackingError)
	{
		this.ref = ref;
		this.act = act;
		this.key = key;
		this.timestamp = timestamp;
		this.actAz = actAz;
		this.actEl = actEl;
		this.actRA = actRA;
		this.actDec = actDec;
		this.trackingError = trackingError;
	}
}

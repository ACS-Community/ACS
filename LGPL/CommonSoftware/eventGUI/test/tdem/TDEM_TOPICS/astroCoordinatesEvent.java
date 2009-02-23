package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "astroCoordinatesEvent"
 *	@author JacORB IDL compiler 
 */

public final class astroCoordinatesEvent
	implements org.omg.CORBA.portable.IDLEntity
{
	public astroCoordinatesEvent(){}
	public double alpha;
	public double delta;
	public int key;
	public long timestamp;
	public astroCoordinatesEvent(double alpha, double delta, int key, long timestamp)
	{
		this.alpha = alpha;
		this.delta = delta;
		this.key = key;
		this.timestamp = timestamp;
	}
}

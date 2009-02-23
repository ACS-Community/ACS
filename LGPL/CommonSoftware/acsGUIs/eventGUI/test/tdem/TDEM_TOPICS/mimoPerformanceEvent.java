package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "mimoPerformanceEvent"
 *	@author JacORB IDL compiler 
 */

public final class mimoPerformanceEvent
	implements org.omg.CORBA.portable.IDLEntity
{
	public mimoPerformanceEvent(){}
	public tdem.TDEM_TOPICS.modalSpace decomposition;
	public double wfError;
	public int key;
	public long timestamp;
	public mimoPerformanceEvent(tdem.TDEM_TOPICS.modalSpace decomposition, double wfError, int key, long timestamp)
	{
		this.decomposition = decomposition;
		this.wfError = wfError;
		this.key = key;
		this.timestamp = timestamp;
	}
}

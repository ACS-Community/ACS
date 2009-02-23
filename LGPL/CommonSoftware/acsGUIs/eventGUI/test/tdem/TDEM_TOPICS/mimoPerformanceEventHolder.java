package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "mimoPerformanceEvent"
 *	@author JacORB IDL compiler 
 */

public final class mimoPerformanceEventHolder
	implements org.omg.CORBA.portable.Streamable
{
	public tdem.TDEM_TOPICS.mimoPerformanceEvent value;

	public mimoPerformanceEventHolder ()
	{
	}
	public mimoPerformanceEventHolder(final tdem.TDEM_TOPICS.mimoPerformanceEvent initial)
	{
		value = initial;
	}
	public org.omg.CORBA.TypeCode _type ()
	{
		return tdem.TDEM_TOPICS.mimoPerformanceEventHelper.type ();
	}
	public void _read(final org.omg.CORBA.portable.InputStream _in)
	{
		value = tdem.TDEM_TOPICS.mimoPerformanceEventHelper.read(_in);
	}
	public void _write(final org.omg.CORBA.portable.OutputStream _out)
	{
		tdem.TDEM_TOPICS.mimoPerformanceEventHelper.write(_out, value);
	}
}

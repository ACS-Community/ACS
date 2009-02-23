package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "pttDataEvent"
 *	@author JacORB IDL compiler 
 */

public final class pttDataEventHolder
	implements org.omg.CORBA.portable.Streamable
{
	public tdem.TDEM_TOPICS.pttDataEvent value;

	public pttDataEventHolder ()
	{
	}
	public pttDataEventHolder(final tdem.TDEM_TOPICS.pttDataEvent initial)
	{
		value = initial;
	}
	public org.omg.CORBA.TypeCode _type ()
	{
		return tdem.TDEM_TOPICS.pttDataEventHelper.type ();
	}
	public void _read(final org.omg.CORBA.portable.InputStream _in)
	{
		value = tdem.TDEM_TOPICS.pttDataEventHelper.read(_in);
	}
	public void _write(final org.omg.CORBA.portable.OutputStream _out)
	{
		tdem.TDEM_TOPICS.pttDataEventHelper.write(_out, value);
	}
}

package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "pttOffsetEvent"
 *	@author JacORB IDL compiler 
 */

public final class pttOffsetEventHolder
	implements org.omg.CORBA.portable.Streamable
{
	public tdem.TDEM_TOPICS.pttOffsetEvent value;

	public pttOffsetEventHolder ()
	{
	}
	public pttOffsetEventHolder(final tdem.TDEM_TOPICS.pttOffsetEvent initial)
	{
		value = initial;
	}
	public org.omg.CORBA.TypeCode _type ()
	{
		return tdem.TDEM_TOPICS.pttOffsetEventHelper.type ();
	}
	public void _read(final org.omg.CORBA.portable.InputStream _in)
	{
		value = tdem.TDEM_TOPICS.pttOffsetEventHelper.read(_in);
	}
	public void _write(final org.omg.CORBA.portable.OutputStream _out)
	{
		tdem.TDEM_TOPICS.pttOffsetEventHelper.write(_out, value);
	}
}

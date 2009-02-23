package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "pointingOffsetEvent"
 *	@author JacORB IDL compiler 
 */

public final class pointingOffsetEventHolder
	implements org.omg.CORBA.portable.Streamable
{
	public tdem.TDEM_TOPICS.pointingOffsetEvent value;

	public pointingOffsetEventHolder ()
	{
	}
	public pointingOffsetEventHolder(final tdem.TDEM_TOPICS.pointingOffsetEvent initial)
	{
		value = initial;
	}
	public org.omg.CORBA.TypeCode _type ()
	{
		return tdem.TDEM_TOPICS.pointingOffsetEventHelper.type ();
	}
	public void _read(final org.omg.CORBA.portable.InputStream _in)
	{
		value = tdem.TDEM_TOPICS.pointingOffsetEventHelper.read(_in);
	}
	public void _write(final org.omg.CORBA.portable.OutputStream _out)
	{
		tdem.TDEM_TOPICS.pointingOffsetEventHelper.write(_out, value);
	}
}

package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "modalOffsetEvent"
 *	@author JacORB IDL compiler 
 */

public final class modalOffsetEventHolder
	implements org.omg.CORBA.portable.Streamable
{
	public tdem.TDEM_TOPICS.modalOffsetEvent value;

	public modalOffsetEventHolder ()
	{
	}
	public modalOffsetEventHolder(final tdem.TDEM_TOPICS.modalOffsetEvent initial)
	{
		value = initial;
	}
	public org.omg.CORBA.TypeCode _type ()
	{
		return tdem.TDEM_TOPICS.modalOffsetEventHelper.type ();
	}
	public void _read(final org.omg.CORBA.portable.InputStream _in)
	{
		value = tdem.TDEM_TOPICS.modalOffsetEventHelper.read(_in);
	}
	public void _write(final org.omg.CORBA.portable.OutputStream _out)
	{
		tdem.TDEM_TOPICS.modalOffsetEventHelper.write(_out, value);
	}
}

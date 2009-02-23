package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "esDataEvent"
 *	@author JacORB IDL compiler 
 */

public final class esDataEventHolder
	implements org.omg.CORBA.portable.Streamable
{
	public tdem.TDEM_TOPICS.esDataEvent value;

	public esDataEventHolder ()
	{
	}
	public esDataEventHolder(final tdem.TDEM_TOPICS.esDataEvent initial)
	{
		value = initial;
	}
	public org.omg.CORBA.TypeCode _type ()
	{
		return tdem.TDEM_TOPICS.esDataEventHelper.type ();
	}
	public void _read(final org.omg.CORBA.portable.InputStream _in)
	{
		value = tdem.TDEM_TOPICS.esDataEventHelper.read(_in);
	}
	public void _write(final org.omg.CORBA.portable.OutputStream _out)
	{
		tdem.TDEM_TOPICS.esDataEventHelper.write(_out, value);
	}
}

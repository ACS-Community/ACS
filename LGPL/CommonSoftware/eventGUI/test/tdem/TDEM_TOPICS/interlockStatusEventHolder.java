package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "interlockStatusEvent"
 *	@author JacORB IDL compiler 
 */

public final class interlockStatusEventHolder
	implements org.omg.CORBA.portable.Streamable
{
	public tdem.TDEM_TOPICS.interlockStatusEvent value;

	public interlockStatusEventHolder ()
	{
	}
	public interlockStatusEventHolder(final tdem.TDEM_TOPICS.interlockStatusEvent initial)
	{
		value = initial;
	}
	public org.omg.CORBA.TypeCode _type ()
	{
		return tdem.TDEM_TOPICS.interlockStatusEventHelper.type ();
	}
	public void _read(final org.omg.CORBA.portable.InputStream _in)
	{
		value = tdem.TDEM_TOPICS.interlockStatusEventHelper.read(_in);
	}
	public void _write(final org.omg.CORBA.portable.OutputStream _out)
	{
		tdem.TDEM_TOPICS.interlockStatusEventHelper.write(_out, value);
	}
}

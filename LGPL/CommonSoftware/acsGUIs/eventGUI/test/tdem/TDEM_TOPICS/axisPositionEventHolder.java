package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "axisPositionEvent"
 *	@author JacORB IDL compiler 
 */

public final class axisPositionEventHolder
	implements org.omg.CORBA.portable.Streamable
{
	public tdem.TDEM_TOPICS.axisPositionEvent value;

	public axisPositionEventHolder ()
	{
	}
	public axisPositionEventHolder(final tdem.TDEM_TOPICS.axisPositionEvent initial)
	{
		value = initial;
	}
	public org.omg.CORBA.TypeCode _type ()
	{
		return tdem.TDEM_TOPICS.axisPositionEventHelper.type ();
	}
	public void _read(final org.omg.CORBA.portable.InputStream _in)
	{
		value = tdem.TDEM_TOPICS.axisPositionEventHelper.read(_in);
	}
	public void _write(final org.omg.CORBA.portable.OutputStream _out)
	{
		tdem.TDEM_TOPICS.axisPositionEventHelper.write(_out, value);
	}
}

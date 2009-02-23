package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "astroCoordinatesEvent"
 *	@author JacORB IDL compiler 
 */

public final class astroCoordinatesEventHolder
	implements org.omg.CORBA.portable.Streamable
{
	public tdem.TDEM_TOPICS.astroCoordinatesEvent value;

	public astroCoordinatesEventHolder ()
	{
	}
	public astroCoordinatesEventHolder(final tdem.TDEM_TOPICS.astroCoordinatesEvent initial)
	{
		value = initial;
	}
	public org.omg.CORBA.TypeCode _type ()
	{
		return tdem.TDEM_TOPICS.astroCoordinatesEventHelper.type ();
	}
	public void _read(final org.omg.CORBA.portable.InputStream _in)
	{
		value = tdem.TDEM_TOPICS.astroCoordinatesEventHelper.read(_in);
	}
	public void _write(final org.omg.CORBA.portable.OutputStream _out)
	{
		tdem.TDEM_TOPICS.astroCoordinatesEventHelper.write(_out, value);
	}
}

package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "sensorSpace"
 *	@author JacORB IDL compiler 
 */

public final class sensorSpaceHolder
	implements org.omg.CORBA.portable.Streamable
{
	public tdem.TDEM_TOPICS.sensorSpace value;

	public sensorSpaceHolder ()
	{
	}
	public sensorSpaceHolder(final tdem.TDEM_TOPICS.sensorSpace initial)
	{
		value = initial;
	}
	public org.omg.CORBA.TypeCode _type ()
	{
		return tdem.TDEM_TOPICS.sensorSpaceHelper.type ();
	}
	public void _read(final org.omg.CORBA.portable.InputStream _in)
	{
		value = tdem.TDEM_TOPICS.sensorSpaceHelper.read(_in);
	}
	public void _write(final org.omg.CORBA.portable.OutputStream _out)
	{
		tdem.TDEM_TOPICS.sensorSpaceHelper.write(_out, value);
	}
}

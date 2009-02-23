package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "actuatorSpace"
 *	@author JacORB IDL compiler 
 */

public final class actuatorSpaceHolder
	implements org.omg.CORBA.portable.Streamable
{
	public tdem.TDEM_TOPICS.actuatorSpace value;

	public actuatorSpaceHolder ()
	{
	}
	public actuatorSpaceHolder(final tdem.TDEM_TOPICS.actuatorSpace initial)
	{
		value = initial;
	}
	public org.omg.CORBA.TypeCode _type ()
	{
		return tdem.TDEM_TOPICS.actuatorSpaceHelper.type ();
	}
	public void _read(final org.omg.CORBA.portable.InputStream _in)
	{
		value = tdem.TDEM_TOPICS.actuatorSpaceHelper.read(_in);
	}
	public void _write(final org.omg.CORBA.portable.OutputStream _out)
	{
		tdem.TDEM_TOPICS.actuatorSpaceHelper.write(_out, value);
	}
}

package tdem.TDEM_TOPICS;

/**
 *	Generated from IDL definition of struct "modalSpace"
 *	@author JacORB IDL compiler 
 */

public final class modalSpaceHolder
	implements org.omg.CORBA.portable.Streamable
{
	public tdem.TDEM_TOPICS.modalSpace value;

	public modalSpaceHolder ()
	{
	}
	public modalSpaceHolder(final tdem.TDEM_TOPICS.modalSpace initial)
	{
		value = initial;
	}
	public org.omg.CORBA.TypeCode _type ()
	{
		return tdem.TDEM_TOPICS.modalSpaceHelper.type ();
	}
	public void _read(final org.omg.CORBA.portable.InputStream _in)
	{
		value = tdem.TDEM_TOPICS.modalSpaceHelper.read(_in);
	}
	public void _write(final org.omg.CORBA.portable.OutputStream _out)
	{
		tdem.TDEM_TOPICS.modalSpaceHelper.write(_out, value);
	}
}

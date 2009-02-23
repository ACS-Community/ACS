package tdem.TDEM_TOPICS;


/**
 *	Generated from IDL definition of struct "actuatorSpace"
 *	@author JacORB IDL compiler 
 */

public final class actuatorSpaceHelper
{
	private static org.omg.CORBA.TypeCode _type = null;
	public static org.omg.CORBA.TypeCode type ()
	{
		if (_type == null)
		{
			_type = org.omg.CORBA.ORB.init().create_struct_tc(tdem.TDEM_TOPICS.actuatorSpaceHelper.id(),"actuatorSpace",new org.omg.CORBA.StructMember[]{new org.omg.CORBA.StructMember("ptt", org.omg.CORBA.ORB.init().create_array_tc(2952,org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.from_int(7))), null)});
		}
		return _type;
	}

	public static void insert (final org.omg.CORBA.Any any, final tdem.TDEM_TOPICS.actuatorSpace s)
	{
		any.type(type());
		write( any.create_output_stream(),s);
	}

	public static tdem.TDEM_TOPICS.actuatorSpace extract (final org.omg.CORBA.Any any)
	{
		return read(any.create_input_stream());
	}

	public static String id()
	{
		return "IDL:tdem/TDEM_TOPICS/actuatorSpace:1.0";
	}
	public static tdem.TDEM_TOPICS.actuatorSpace read (final org.omg.CORBA.portable.InputStream in)
	{
		tdem.TDEM_TOPICS.actuatorSpace result = new tdem.TDEM_TOPICS.actuatorSpace();
		result.ptt = new double[2952];
		in.read_double_array(result.ptt,0,2952);
		return result;
	}
	public static void write (final org.omg.CORBA.portable.OutputStream out, final tdem.TDEM_TOPICS.actuatorSpace s)
	{
				if (s.ptt.length<2952)
			throw new org.omg.CORBA.MARSHAL("Incorrect array size "+s.ptt.length+", expecting 2952");
		out.write_double_array(s.ptt,0,2952);
	}
}

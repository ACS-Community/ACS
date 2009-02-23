package tdem.TDEM_TOPICS;


/**
 *	Generated from IDL definition of struct "sensorSpace"
 *	@author JacORB IDL compiler 
 */

public final class sensorSpaceHelper
{
	private static org.omg.CORBA.TypeCode _type = null;
	public static org.omg.CORBA.TypeCode type ()
	{
		if (_type == null)
		{
			_type = org.omg.CORBA.ORB.init().create_struct_tc(tdem.TDEM_TOPICS.sensorSpaceHelper.id(),"sensorSpace",new org.omg.CORBA.StructMember[]{new org.omg.CORBA.StructMember("es", org.omg.CORBA.ORB.init().create_array_tc(5604,org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.from_int(7))), null)});
		}
		return _type;
	}

	public static void insert (final org.omg.CORBA.Any any, final tdem.TDEM_TOPICS.sensorSpace s)
	{
		any.type(type());
		write( any.create_output_stream(),s);
	}

	public static tdem.TDEM_TOPICS.sensorSpace extract (final org.omg.CORBA.Any any)
	{
		return read(any.create_input_stream());
	}

	public static String id()
	{
		return "IDL:tdem/TDEM_TOPICS/sensorSpace:1.0";
	}
	public static tdem.TDEM_TOPICS.sensorSpace read (final org.omg.CORBA.portable.InputStream in)
	{
		tdem.TDEM_TOPICS.sensorSpace result = new tdem.TDEM_TOPICS.sensorSpace();
		result.es = new double[5604];
		in.read_double_array(result.es,0,5604);
		return result;
	}
	public static void write (final org.omg.CORBA.portable.OutputStream out, final tdem.TDEM_TOPICS.sensorSpace s)
	{
				if (s.es.length<5604)
			throw new org.omg.CORBA.MARSHAL("Incorrect array size "+s.es.length+", expecting 5604");
		out.write_double_array(s.es,0,5604);
	}
}

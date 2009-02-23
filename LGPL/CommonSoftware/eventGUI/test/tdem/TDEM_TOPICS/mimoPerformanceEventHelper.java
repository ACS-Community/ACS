package tdem.TDEM_TOPICS;


/**
 *	Generated from IDL definition of struct "mimoPerformanceEvent"
 *	@author JacORB IDL compiler 
 */

public final class mimoPerformanceEventHelper
{
	private static org.omg.CORBA.TypeCode _type = null;
	public static org.omg.CORBA.TypeCode type ()
	{
		if (_type == null)
		{
			_type = org.omg.CORBA.ORB.init().create_struct_tc(tdem.TDEM_TOPICS.mimoPerformanceEventHelper.id(),"mimoPerformanceEvent",new org.omg.CORBA.StructMember[]{new org.omg.CORBA.StructMember("decomposition", tdem.TDEM_TOPICS.modalSpaceHelper.type(), null),new org.omg.CORBA.StructMember("wfError", org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.from_int(7)), null),new org.omg.CORBA.StructMember("key", org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.from_int(3)), null),new org.omg.CORBA.StructMember("timestamp", alma.ACS.TimeIntervalHelper.type(), null)});
		}
		return _type;
	}

	public static void insert (final org.omg.CORBA.Any any, final tdem.TDEM_TOPICS.mimoPerformanceEvent s)
	{
		any.type(type());
		write( any.create_output_stream(),s);
	}

	public static tdem.TDEM_TOPICS.mimoPerformanceEvent extract (final org.omg.CORBA.Any any)
	{
		return read(any.create_input_stream());
	}

	public static String id()
	{
		return "IDL:tdem/TDEM_TOPICS/mimoPerformanceEvent:1.0";
	}
	public static tdem.TDEM_TOPICS.mimoPerformanceEvent read (final org.omg.CORBA.portable.InputStream in)
	{
		tdem.TDEM_TOPICS.mimoPerformanceEvent result = new tdem.TDEM_TOPICS.mimoPerformanceEvent();
		result.decomposition=tdem.TDEM_TOPICS.modalSpaceHelper.read(in);
		result.wfError=in.read_double();
		result.key=in.read_long();
		result.timestamp=in.read_longlong();
		return result;
	}
	public static void write (final org.omg.CORBA.portable.OutputStream out, final tdem.TDEM_TOPICS.mimoPerformanceEvent s)
	{
		tdem.TDEM_TOPICS.modalSpaceHelper.write(out,s.decomposition);
		out.write_double(s.wfError);
		out.write_long(s.key);
		out.write_longlong(s.timestamp);
	}
}

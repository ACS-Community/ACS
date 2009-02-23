package tdem.TDEM_TOPICS;


/**
 *	Generated from IDL definition of struct "interlockStatusEvent"
 *	@author JacORB IDL compiler 
 */

public final class interlockStatusEventHelper
{
	private static org.omg.CORBA.TypeCode _type = null;
	public static org.omg.CORBA.TypeCode type ()
	{
		if (_type == null)
		{
			_type = org.omg.CORBA.ORB.init().create_struct_tc(tdem.TDEM_TOPICS.interlockStatusEventHelper.id(),"interlockStatusEvent",new org.omg.CORBA.StructMember[]{new org.omg.CORBA.StructMember("motionStop1", org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.from_int(8)), null),new org.omg.CORBA.StructMember("motionStop2", org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.from_int(8)), null),new org.omg.CORBA.StructMember("emergencyStop1", org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.from_int(8)), null),new org.omg.CORBA.StructMember("key", org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.from_int(3)), null),new org.omg.CORBA.StructMember("timestamp", alma.ACS.TimeIntervalHelper.type(), null)});
		}
		return _type;
	}

	public static void insert (final org.omg.CORBA.Any any, final tdem.TDEM_TOPICS.interlockStatusEvent s)
	{
		any.type(type());
		write( any.create_output_stream(),s);
	}

	public static tdem.TDEM_TOPICS.interlockStatusEvent extract (final org.omg.CORBA.Any any)
	{
		return read(any.create_input_stream());
	}

	public static String id()
	{
		return "IDL:tdem/TDEM_TOPICS/interlockStatusEvent:1.0";
	}
	public static tdem.TDEM_TOPICS.interlockStatusEvent read (final org.omg.CORBA.portable.InputStream in)
	{
		tdem.TDEM_TOPICS.interlockStatusEvent result = new tdem.TDEM_TOPICS.interlockStatusEvent();
		result.motionStop1=in.read_boolean();
		result.motionStop2=in.read_boolean();
		result.emergencyStop1=in.read_boolean();
		result.key=in.read_long();
		result.timestamp=in.read_longlong();
		return result;
	}
	public static void write (final org.omg.CORBA.portable.OutputStream out, final tdem.TDEM_TOPICS.interlockStatusEvent s)
	{
		out.write_boolean(s.motionStop1);
		out.write_boolean(s.motionStop2);
		out.write_boolean(s.emergencyStop1);
		out.write_long(s.key);
		out.write_longlong(s.timestamp);
	}
}

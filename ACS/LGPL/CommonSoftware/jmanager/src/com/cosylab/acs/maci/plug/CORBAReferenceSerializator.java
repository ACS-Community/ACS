package com.cosylab.acs.maci.plug;

/*
 * @@COPYRIGHT@@
 */
 
import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;

/**
 * ORB holder class.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class CORBAReferenceSerializator
{

	/**
	 * CORBA ORB.
	 */
	private static ORB orb = null;
	
	/**
	 * Return the CORBA ORB.
	 * @return ORB
	 */
	public static ORB getOrb()
	{
		return orb;
	}

	/**
	 * Set the CORBA ORB.
	 * @param orb	CORBA ORB
	 */
	public static void setOrb(ORB orb)
	{
		CORBAReferenceSerializator.orb = orb;
	}
	
	/**
	 * Serialize CORBA reference.
	 * @param reference CORBA object reference
	 * @return	serialized CORBA object reference
	 */
	public static String serialize(Object reference)
	{
		if (CORBAReferenceSerializator.orb == null || reference == null)
			return null;

		return orb.object_to_string(reference);			
	}

	/**
	 * Deserialize CORBA reference.
	 * @param ior serialized CORBA object reference
	 * @return reference CORBA object reference
	 */
	public static Object deserialize(String ior)
	{
		if (CORBAReferenceSerializator.orb == null || ior == null)
			return null;
			
		return orb.string_to_object(ior);
	}
}

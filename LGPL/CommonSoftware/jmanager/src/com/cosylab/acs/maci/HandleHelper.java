/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci;

/**
 * Handle helper class providing various utilities.
 *  
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class HandleHelper implements HandleConstants
{
	
	/**
	 * Returns a single-line human readable rendition of the handle.
	 * 
	 * @param	handle	handle to be rendered
	 * @return single-line human readable rendition of the handle
	 */
	public static String toString(int handle)
	{
		StringBuffer sbuff = new StringBuffer();
		sbuff.append("Handle (0x");
		sbuff.append(Integer.toHexString(handle));
		sbuff.append(") = { ");
		sbuff.append("type = ");
		switch	(handle & TYPE_MASK)
		{
			case CONTAINER_MASK:
				sbuff.append("CONTAINER");
				break;
			case CLIENT_MASK:
				sbuff.append("CLIENT");
				break;
			case ADMINISTRATOR_MASK:
				sbuff.append("ADMINISTRATOR");
				break;
			case COMPONENT_MASK:
				sbuff.append("COMPONENT");
				break;
			default:
				sbuff.append("unknown (0x");
				sbuff.append(Integer.toHexString(handle & TYPE_MASK));
				sbuff.append(")");
			
		}
		sbuff.append(", key = (0x");
		sbuff.append(Integer.toHexString((handle & KEY_MASK) >> 16));
		sbuff.append("), id = (0x");
		sbuff.append(Integer.toHexString(handle & HANDLE_MASK));
		sbuff.append(") }");
		return new String(sbuff);

	}



}

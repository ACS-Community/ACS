/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci;

/**
 * The Handle alias defines a universal handle used by MACI to uniquely tag the object instances.
 * 
 * Handles optimize access and identifications of MACI components and replace the use of string identifiers.
 * Handles are not unique globally and are not static. Manager guarantees only that they are unique in their scope
 * and only for the duration of the object denoted by the handle.
 * 
 * Upper 8 bits (24 to 31) of the handle denote the type of the object that the handle is representing.
 * Possible values of the upper-most byte are:
 * 	<UL>
 * 	<LI>0 - the handle is reserved</LI>
 * 	<LI>1 - the handle represents a component</LI>
 * 	<LI>2 - the handle represents a client</LI>
 * 	<LI>3 - the handle represents an administrator</LI>
 * 	<LI>4 - the handle represents an container</LI>
 * 	<LI>5 - the handle represents a Manager (for inter-domain communcation).</LI>
 * 	</UL>
 * 	
 * Bits 0 thru 15 uniquely tag object instance. Bits 16 thru 23 are reserved.
 *  
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public interface HandleConstants
{

	/**
	 * Component handle mask.
	 */
	public static final int COMPONENT_MASK = 0x01000000;

	/**
	 * Client handle mask.
	 */
	public static final int CLIENT_MASK = 0x02000000;

	/**
	 * Administrator handle mask.
	 */
	public static final int ADMINISTRATOR_MASK = 0x03000000;

	/**
	 * Container handle mask.
	 */
	public static final int CONTAINER_MASK = 0x04000000;

	/**
	 * Manager handle mask.
	 */
	public static final int MANAGER_MASK = 0x05000000;
	
	/**
	 * Type mask of the handle.
	 */
	public static final int TYPE_MASK = 0xFF000000;

	/**
	 * Key mask of the handle.
	 */
	public static final int KEY_MASK = 0x00FF0000;
	
	/**
	 * Instance ID part of handle.
	 */
	public static final int HANDLE_MASK = 0x0000FFFF;


}

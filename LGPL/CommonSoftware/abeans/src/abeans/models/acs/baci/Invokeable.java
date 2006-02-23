/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci;

import abeans.core.Identifiable;
import abeans.engine.Database;

import com.cosylab.datatypes.AsynchronousAccess;

/**
 * All BACI modeling instances able to invoke methods should implement this interface.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public interface Invokeable extends AsynchronousAccess, Identifiable {
	
	/**
	 * Gets the database implementation for the 'invokable' object.
	 */
	Database getDatabase();

}

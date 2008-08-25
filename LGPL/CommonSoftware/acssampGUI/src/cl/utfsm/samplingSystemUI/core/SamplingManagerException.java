/**
 *      @author Julio Araya (jaray[at]alumnos.inf.utfsm.cl) &
 *      Nicolas Troncoso (ntroncos[at]alumnos.inf.utfsm.cl)
 **/

package cl.utfsm.samplingSystemUI.core;

import java.lang.Exception;

/**
* Custom exception class, not much more than that
*/
public class SamplingManagerException extends Exception {

	private static final long serialVersionUID = 2L;

	/**
	* Generic constructor, same as Exception
	* @see Exception
	*/
	public SamplingManagerException() {
		super();
	}

	/**
	* Adds a message to the Generic Exception
	* @param msg Message to be added to the regular Exception
	*/
	public SamplingManagerException(String msg) {
		super(msg+"\n");
	}

	/**
	* Another extension of Exception, now also adds a Generic Exception to the mix
	* @param msg Message to be added to the Exceotion
	* @param e Generic Exception to be attached
	*/
	public SamplingManagerException(String msg, Exception e) {
		super(msg+"\n",e);
	}
}

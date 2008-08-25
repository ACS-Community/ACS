/**
 *      @author Julio Araya (jaray[at]alumnos.inf.utfsm.cl) &
 *      Nicolas Troncoso (ntroncos[at]alumnos.inf.utfsm.cl)
 **/

package cl.utfsm.samplingSystemUI.core;
import java.lang.Exception;


public class AcsInformationException extends Exception {

	private static final long serialVersionUID = 7751725699360884525L;

	public AcsInformationException() {
		super();
	}

	/**
	* Adds a String to the generic ACSInformationException
	*
	* @param msg The String to be added
	**/
	public AcsInformationException(String msg) {
		super(msg+"\n");
	}

	/**
	* Adds a message to the given Exception, then passes it to the generic ACSInformationException
	*
	* @param msg The string to be added
	* @param e Exception to be passed to the parent
	**/
	public AcsInformationException(String msg, Exception e) {
		super(msg+"\n",e);
	}
}

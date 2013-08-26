/**
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 * @author Jorge Avarias (javarias[at]alumnos.inf.utfsm.cl)
 */

package cl.utfsm.acs.types;

/** A simple object is a TypedObject that implements an AcsSimpleType
 * definition. Currently the restrictions are not implemented.
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 */
public class SimpleObject extends TypedObject {
        //@SuppressWarnings("hiding")
        /** The object type */
	protected AcsSimpleType sType;
        /** Only defines the type of the object
         * @param type the object type (simple)
         */
        public SimpleObject(AcsSimpleType type){
		this.sType=type;
                this.type=type;
        }
        /** Get the restriction information. Not used Yet
         * @return the restriction information.
         */
        public String getRestriction(){
                return(sType.restriction);
        }

}



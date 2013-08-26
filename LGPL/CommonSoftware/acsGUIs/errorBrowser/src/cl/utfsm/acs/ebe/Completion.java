/**
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 * @author Jorge Avarias (javarias[at]alumnos.inf.utfsm.cl)
 */

package cl.utfsm.acs.ebe;
import cl.utfsm.acs.types.AcsComplexType;
import cl.utfsm.acs.types.ComplexObject;

/** The internal representation of a Completion.
 * Almost no diference with a plain Complex Object,
 * just defined for naming reasons.
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 */
public class Completion extends ComplexObject {
        /** The class AcsType*/
        protected static AcsComplexType completionType;
        /** The class method to setup the type */
        public static void setClassType(AcsComplexType t){
                completionType=t;
        }
        /** The class method to get the class Type 
         * @return The class type
         */
        public static AcsComplexType getClassType(){
                return(completionType);
        }
        /** Completion constructor */
        public Completion(){
                super(completionType);
        }
}


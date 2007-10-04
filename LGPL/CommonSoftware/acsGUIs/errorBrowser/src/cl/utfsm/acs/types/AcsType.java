/**
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 * @author Jorge Avarias (javarias[at]alumnos.inf.utfsm.cl)
 */

package cl.utfsm.acs.types;

/** An abstract class for the type ACS XML definitions.
 * This class only set the common attriubutes that a types should have.
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 */
public abstract class AcsType{
        /** The namespace defined in a schema file */
        public String namespace;
        /** The name of the type definition */ 
        public String name;
        /** The documentation of the type (very usefull) */
        public String documentation;
        /** Setup the AcsType values
         * @param namespace Every type has a namespace
         * @param name The name of the type definition
         * @param documentation Some information of the type
         */
        public AcsType(String namespace,String name,String documentation){
                this.namespace=namespace;
                this.name=name;
                this.documentation=documentation;
        }
}



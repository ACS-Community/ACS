/**
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 * @author Jorge Avarias (javarias[at]alumnos.inf.utfsm.cl)
 */

package cl.utfsm.acs.types;

/** A simple ACS XML type, that includes a restriction definition.
 * This is just a simple type, that includes the simple type definition,
 * and a restriction string.
 * @see AcsType
 */
public class AcsSimpleType extends AcsType{
        /** A Restriction string. This is attribute has no use right now */
        public String restriction;
        /** Fill the simple type with the needed information 
         * @param namespace Every type has a namespace
         * @param name The name of the type definition
         * @param documentation Some information of the type
         * @param restriction The string restrictions (unused)
         */
        public AcsSimpleType(String namespace,String name,String documentation,String restriction){
                super(namespace,name,documentation);
                this.restriction=restriction;
        }
}

